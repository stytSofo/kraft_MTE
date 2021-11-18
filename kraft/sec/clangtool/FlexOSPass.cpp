// Declares clang::SyntaxOnlyAction.
#include "clang/Frontend/FrontendActions.h"
#include "clang/Tooling/CommonOptionsParser.h"
#include "clang/Tooling/Tooling.h"
// Declares llvm::cl::extrahelp.
#include "llvm/Support/CommandLine.h"
#include "clang/ASTMatchers/ASTMatchers.h"
#include "clang/ASTMatchers/ASTMatchFinder.h"
// Declares Rewriter
#include "clang/Rewrite/Core/Rewriter.h"
// Declares RefactoringTool
#include "clang/Tooling/Refactoring.h"

#include <sstream>

using namespace clang;
using namespace clang::ast_matchers;
using namespace clang::tooling;
using namespace llvm;

static llvm::cl::OptionCategory MyToolCategory("flexos-rewriter options");

/* enable or disable the data shadow stack, when disabled, annotated stack
 * allocations are replaced with heap allocations */
static cl::opt<bool> EnableDSSOption("dss", cl::cat(MyToolCategory));

static cl::extrahelp CommonHelp(CommonOptionsParser::HelpMessage);
static cl::extrahelp MoreHelp("\nThis is FlexOS's clang frontend pass."
                              "Code is documentation. :)\n");

/* These two matchers target functions that contain at least one variable to be
 * allocated on the data shadow stack/on the heap. The first one handles the case
 * where a function does not feature any return statement, the second one the
 * remaining cases.
 *
 * note: in these matchers we are not interested to mark the declaration itself,
 * we have a dedicated rule for that.
 */
DeclarationMatcher FuncMatcher =
  // match functions
  functionDecl(
    // that have a body
    hasBody(compoundStmt(
      // and contain
      hasAnySubstatement(
        // a declaration statement
        declStmt(
          // that features a single declaration
          hasSingleDecl(
            // featuring an attribute
            hasAttr(
              // whose class is 'annotate', e.g. [[clang::annotate("whitelist(...)")]]
              clang::attr::Annotate
            )
          )
        )
      )
    // make sure to also remember the function itself
    ).bind("whitelistedFunc")
  )
);
// note: in this matcher we are not interested in the function, otherwise we'd
// visit it multiple times which is no good
DeclarationMatcher ReturnMatcher =
  // match functions
  functionDecl(
    // that have a body
    hasBody(compoundStmt(
      // and contain
      hasAnySubstatement(
        // a declaration statement
        declStmt(
          // that features a single declaration
          hasSingleDecl(
            // featuring an attribute
            hasAttr(
              // whose class is 'annotate', e.g. [[clang::annotate("whitelist(...)")]]
              clang::attr::Annotate
            )
          )
        )
      ),
      // search for return statements (all of them! `has()` would only give one)
      findAll(
        returnStmt().bind("whitelistedReturn")
      )
    )
  )
);

/* This matcher targets all references of variables that are to be stored on the
 * data shadow stack / heap. These have to be transformed (add a *).
 */
StatementMatcher RefMatcher =
  // match references to (declared) variables
  declRefExpr(to(
    // whose declaration
    varDecl(
      // has a local scope
      hasLocalStorage(),
      // and features an attribute
      hasAttr(
        // whose class is 'annotate'
        clang::attr::Annotate
      )
    )
  )
// remember these references, we need to update them
).bind("whitelistedVar");

/* This matcher targets all declarations of (local) variables that must be
 * stored on the DSS / heap. */
StatementMatcher LocalDeclMatcher =
  // match declaration statements
  declStmt(
    // that features a single declaration
    hasSingleDecl(anything()),
    // which features
    containsDeclaration(0, varDecl(
      // a local scope
      hasLocalStorage(),
      // and an attribute
      hasAttr(
        // whose class is 'annotate', e.g. [[clang::annotate("whitelist(...)")]]
        clang::attr::Annotate
      )
    )
  )
// make sure to remember all of these declarations
).bind("whitelistedLocalDecl");

/* This matcher targets all declarations of (global) variables that must be
 * stored in a shared section. */
DeclarationMatcher GlobalDeclMatcher =
  // match declaration statements
  varDecl(
    // with a global scope
    hasGlobalStorage(),
    // and an attribute
    hasAttr(
      // whose class is 'annotate', e.g. [[clang::annotate("whitelist(...)")]]
      clang::attr::Annotate
    )
// make sure to remember all of these declarations
).bind("whitelistedGlobalDecl");

Rewriter TheRewriter;
bool TheRewriterInitialized;

/* This class' run() method is called whenever there is a match, in which case
 * we rewrite the kernel's code appropriately. */
class MatchPrinter : public MatchFinder::MatchCallback {

  void rewriteCS(const CompoundStmt *CS, const MatchFinder::MatchResult &Result) {
    SourceManager &srcMgr = Result.Context->getSourceManager();
    const LangOptions &langOpts = Result.Context->getLangOpts();
    /* This function (or rather, this function's body) contains declarations
     * of variables that we must rewrite. If the DSS is enabled we don't have
     * anything to do here as we only have to transform declarations and
     * references themselves. If it is disabled, then we need to modify the
     * prologue and epilogue (we move variables from the stack to the heap).
     */
    if (!EnableDSSOption.getValue()) {
      const Stmt *FirstStatement = CS->body_front();
      SourceLocation sl = FirstStatement->getBeginLoc();

      if (TheRewriter.InsertText(sl, "/* Pointer list (stack -> heap transformation) */\n"
            "void *_alloced_ptrs[32];\nint _alloced_pos = 0;\n/* --- */\n", false, true))
        llvm::errs() << "Failed to add ptr list (stack -> heap transformation) at " <<
            sl.printToString(srcMgr) << "!\n";

      const Stmt *LastStatement = CS->body_back();

      if (!LastStatement) {
          llvm::errs() << "Unable to find last statement for CompoundStatement:\n";
          CS->dump();
      }

      auto StatementClass = LastStatement->getStmtClass();
      // if it is not a return statement, we must free all variables that
      // we moved from the stack to the heap/DSS
      if (StatementClass != Stmt::ReturnStmtClass) {
        std::string indentation = clang::Lexer::getIndentationForLine(
            LastStatement->getBeginLoc(), srcMgr).str();
        std::string reset = indentation + std::string("/* free variables that we moved from the stack ") +
            std::string("to the heap */\n") + indentation +
            std::string("for (int n = 0; n < _alloced_pos; n++) { uk_free(flexos_shared_alloc, _alloced_ptrs[n]); }\n");
        if (TheRewriter.InsertTextBefore(CS->getEndLoc(), reset)) {
          llvm::errs() << "Failed to add free calls (stack -> heap transformation) at " <<
              CS->getEndLoc().printToString(srcMgr) << "!\n";
        }
      }
    }
  }

  void rewriteDecl(const DeclStmt *D, const MatchFinder::MatchResult &Result) {
    SourceManager &srcMgr = Result.Context->getSourceManager();
    const LangOptions &langOpts = Result.Context->getLangOpts();
    // get VarDecl child and its type
    const VarDecl *VD = (const VarDecl *) D->getSingleDecl();
    const clang::QualType T = VD->getType();
    // get source range: this is the range we want to rewrite
    SourceRange sr = D->getSourceRange();

    /* Make sure not to remove annotations that do not belong to us (e.g.,
     * __attribute__((unused))). Before changing anything, iterate through
     * attributes and save anything that's not ours. */
    std::string remainingAnnotations = "";
    for (auto attr : VD->getAttrs()) {
      std::string attr_whitelist("annotate(\"flexos_whitelist\")");
      std::string SS; llvm::raw_string_ostream S(SS);
      attr->printPretty(S, PrintingPolicy(LangOptions()));
      if (S.str().find(attr_whitelist) == std::string::npos) {
          remainingAnnotations.append(S.str());
      }
    }

    std::ostringstream os;
    /* declarations are somewhat complicated: we need to handle at least
     * constant size arrays, variable size arrays, and primitive data types
     * separately. */
    if (T.getTypePtr()->isConstantArrayType()) {
      /* Constant size arrays */
      const ConstantArrayType *CAT = Result.Context->getAsConstantArrayType(T);
      if (!EnableDSSOption.getValue()) {
        // note: no ';\n', we are replacing so it's already there
        os << CAT->getElementType().getAsString() << " **" << VD->getName().str() <<
            " " << remainingAnnotations <<
            " = (" << CAT->getElementType().getLocalUnqualifiedType().getAsString() << "**) uk_malloc(flexos_shared_alloc, (" <<
            CAT->getSize().getLimitedValue() << ") * sizeof(" <<
            CAT->getElementType().getLocalUnqualifiedType().getAsString() << "))";
      } else {
        os << "\n" << CAT->getElementType().getAsString() << " ** _dss_" << VD->getName().str() <<
            " " << remainingAnnotations << " = (" <<
            CAT->getElementType().getLocalUnqualifiedType().getAsString() <<
            "**) (((uintptr_t) &" << VD->getName().str() << ") + STACK_SIZE);";
      }
    } else if (T.getTypePtr()->isVariableArrayType()) {
      /* Variable size arrays */
      const VariableArrayType *VAT = Result.Context->getAsVariableArrayType(T);
      if (!EnableDSSOption.getValue()) {
        std::string statement; raw_string_ostream stream(statement);
        VAT->getSizeExpr()->printPretty(stream, nullptr, PrintingPolicy(LangOptions()));
        // note: no ';\n', we are replacing so it's already there
        os << VAT->getElementType().getAsString() << " **" << VD->getName().str() <<
            " " << remainingAnnotations <<
            " = (" << VAT->getElementType().getLocalUnqualifiedType().getAsString() << "**) uk_malloc(flexos_shared_alloc, (" <<
            stream.str() << ") * sizeof(" << VAT->getElementType().getLocalUnqualifiedType().getAsString() << "))";
      } else {
        // same as constant size arrays, no difference in the case of the DSS
        os << "\n" << VAT->getElementType().getAsString() << " ** _dss_" << VD->getName().str() <<
            " " << remainingAnnotations << " = (" <<
            VAT->getElementType().getLocalUnqualifiedType().getAsString() <<
            "**) (((uintptr_t) &" << VD->getName().str() << ") + STACK_SIZE);";
      }
    } else {
      /* Primitive types */
      if (!EnableDSSOption.getValue()) {
        os << VD->getType().getAsString() << " *" << VD->getName().str() <<
            " " << remainingAnnotations <<
            " = (" << VD->getType().getLocalUnqualifiedType().getAsString() << "*) uk_malloc(flexos_shared_alloc, " <<
            "sizeof(" << VD->getType().getLocalUnqualifiedType().getAsString() << "))";
      } else {
        os << "\n" << VD->getType().getAsString() << " * _dss_" << VD->getName().str() <<
            " " << remainingAnnotations << " = (" <<
            VD->getType().getLocalUnqualifiedType().getAsString() <<
            "*) (((uintptr_t) &" << VD->getName().str() << ") + STACK_SIZE);";
      }
    }

    bool failure;
    if (!EnableDSSOption.getValue()) {
      failure = TheRewriter.ReplaceText(sr, os.str());
    } else {
      SourceLocation next_loc(clang::Lexer::getLocForEndOfToken(
          D->getEndLoc(), 0, srcMgr, langOpts));
      failure = TheRewriter.InsertText(next_loc, os.str(), true, true);
    }

    if (failure) {
      llvm::errs() << "Failed to rewrite variable " << VD->getName() << " at " <<
          D->getBeginLoc().printToString(srcMgr) << "!\n";
    }

    std::ostringstream os2;
    if (!EnableDSSOption.getValue()) {
      /* if we are allocating on the heap, we must update the ptr list */
      os2 << ";\n_alloced_ptrs[_alloced_pos++] = " << VD->getName().str() << ";";
    }

    if (VD->hasInit()) {
      /* handle potential initializations */
      const Expr *Initializer = VD->getAnyInitializer();
      std::string statement; raw_string_ostream stream(statement);
      Initializer->printPretty(stream, nullptr, PrintingPolicy(LangOptions()));
      if (!EnableDSSOption.getValue()) {
        os2 << "\n(*" << VD->getName().str() << ") = " << stream.str() << ";";
      } else {
        os2 << "\n(* _dss_" << VD->getName().str() << ") = " << stream.str() << ";";
      }
    }

    SourceLocation next_loc(clang::Lexer::getLocForEndOfToken(
        D->getEndLoc(), 0, srcMgr, langOpts));
    if (TheRewriter.InsertText(next_loc, os2.str(), true, true)) {
      llvm::errs() << "Failed to write initializer for variable " <<
          VD->getName() << " at " << D->getBeginLoc().printToString(srcMgr) << "!\n";
    }
  }

  virtual void run(const MatchFinder::MatchResult &Result) {
    SourceManager &srcMgr = Result.Context->getSourceManager();
    const LangOptions &langOpts = Result.Context->getLangOpts();
    if (!TheRewriterInitialized) {
      // probably not the cleanest way to initialize the Rewriter,
      // but, hey, it works. We should probably use Replacements to
      // be clean there.
      TheRewriter = Rewriter(srcMgr, langOpts);
      TheRewriterInitialized = true;
    }

    /* This is a function that contains variables to be transformed. */
    if (const CompoundStmt *CS = Result.Nodes.getNodeAs<clang::CompoundStmt>("whitelistedFunc")) {
      this->rewriteCS(CS, Result);
    }

    /* This is a global variable that must be allocated in a shared section.
     * Rewrite the annotation to section(...). */
    if (const VarDecl *VD = Result.Nodes.getNodeAs<clang::VarDecl>("whitelistedGlobalDecl")) {
      for (auto attr : VD->getAttrs()) {
        // make sure that we don't remove annotation that do not belong to us
        std::string attr_whitelist("annotate(\"flexos_whitelist\")");
        std::string SS; llvm::raw_string_ostream S(SS);
        attr->printPretty(S, PrintingPolicy(LangOptions()));
        if (S.str().find(attr_whitelist) != std::string::npos) {
          SourceRange sr = attr->getRange();
          if (TheRewriter.ReplaceText(sr, "section(\".shared\")")) {
            llvm::errs() << "Failed to remove attribute " << attr->getSpelling() <<
              " at " << attr->getLoc().printToString(srcMgr) << "!\n";
          }
        }
      }
    }

    /* This is a variable declaration that must be moved away from the stack:
     * allocate it on the heap/DSS and remove whitelist annotation. */
    if (const DeclStmt *D = Result.Nodes.getNodeAs<clang::DeclStmt>("whitelistedLocalDecl")) {
      this->rewriteDecl(D, Result);
    }

    /* This is a reference to a variable on the DSS/heap: it is now a pointer, add
     * a '*' to deference it. */
    if (const DeclRefExpr *DRE = Result.Nodes.getNodeAs<clang::DeclRefExpr>("whitelistedVar")) {
        /* note: we need this .getFileLoc() trickery to support macros,
         * otherwise the area will be non-rewritable. */
        SourceLocation startLoc = TheRewriter.getSourceMgr().getFileLoc(
            DRE->getBeginLoc());
        SourceLocation endLoc = TheRewriter.getSourceMgr().getFileLoc(
            DRE->getEndLoc());
        SourceRange Range(startLoc, endLoc);

        std::ostringstream os;
        std::string VarName = DRE->getDecl()->getName().str();
        if (!EnableDSSOption.getValue()) {
          os << "(*" << VarName << ")";
        } else {
          os << "(* _dss_" << VarName << ")";
        }

        if (TheRewriter.ReplaceText(Range, os.str()))
          llvm::errs() << "Failed to replace with '" << os.str() << "' at " <<
              startLoc.printToString(srcMgr) << "!\n";
    }

    /* This is a return statement in a function that required stack -> heap
     * transformations, add calls to free(). */
    if (const ReturnStmt *RS = Result.Nodes.getNodeAs<clang::ReturnStmt>("whitelistedReturn")) {
      if (!EnableDSSOption.getValue()) {
        SourceLocation sl = RS->getBeginLoc();
        std::string reset = "/* free variables that we moved from the stack to the heap */\n"
            "for (int n = 0; n < _alloced_pos; n++) { uk_free(flexos_shared_alloc, _alloced_ptrs[n]); }\n";
        if (TheRewriter.InsertText(sl, reset, false, true))
          llvm::errs() << "Failed to add free calls (stack -> heap transformation) at " <<
              sl.printToString(srcMgr) << "!\n";
      }
    }

    // save changes in-place
    TheRewriter.overwriteChangedFiles();
  }
};

int pass1(int argc, const char **argv) {
  auto ExpectedParser = CommonOptionsParser::create(argc, argv, MyToolCategory);
  if (!ExpectedParser) {
    // Fail gracefully for unsupported options.
    llvm::errs() << ExpectedParser.takeError();
    return 1;
  }

  CommonOptionsParser& OptionsParser = ExpectedParser.get();

  RefactoringTool Tool(OptionsParser.getCompilations(),
      OptionsParser.getSourcePathList());

  MatchPrinter Printer;
  MatchFinder Finder;

  /* In this first pass we modify functions that contain annotated declarations
   * to create ptr lists and add free() calls in prologue and epilogue. */
  Finder.addMatcher(FuncMatcher, &Printer);
  Finder.addMatcher(ReturnMatcher, &Printer);

  return Tool.run(newFrontendActionFactory(&Finder).get());
}

int pass2(int argc, const char **argv) {
  auto ExpectedParser = CommonOptionsParser::create(argc, argv, MyToolCategory);
  if (!ExpectedParser) {
    // Fail gracefully for unsupported options.
    llvm::errs() << ExpectedParser.takeError();
    return 1;
  }

  CommonOptionsParser& OptionsParser = ExpectedParser.get();

  RefactoringTool Tool(OptionsParser.getCompilations(),
      OptionsParser.getSourcePathList());

  MatchPrinter Printer;
  MatchFinder Finder;

  /* In this second pass we modify annotated declarations to allocate them on
   * the DSS, and modify references to these variable accordingly. We also
   * handle shared global variables. */
  Finder.addMatcher(LocalDeclMatcher, &Printer);
  Finder.addMatcher(RefMatcher, &Printer);
  Finder.addMatcher(GlobalDeclMatcher, &Printer);

  return Tool.run(newFrontendActionFactory(&Finder).get());
}

int main(int argc, const char **argv) {
  /* We split this tool into multiple passes to force AST update. This allows us
   * to perform changes that would otherwise conflict in a single pass. */

  llvm::outs() << "[I] Running first pass...\n";
  pass1(argc, argv);

  // make sure to recreate a fresh Rewriter object for the new pass
  TheRewriterInitialized = false;

  llvm::outs() << "[I] Running second pass...\n";
  pass2(argc, argv);

  return 0;
}
