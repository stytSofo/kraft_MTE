@mallocreplacer@
expression size;
@@
- flexos_malloc_whitelist(size, ...)
+ uk_malloc(flexos_shared_alloc, size)

@reallocreplacer@
expression var;
expression size;
@@
- flexos_realloc_whitelist(var, size, ...)
+ uk_realloc(flexos_shared_alloc, var, size)

@callocreplacer@
expression size;
expression nmemb;
@@
- flexos_calloc_whitelist(nmemb, size, ...)
+ uk_calloc(flexos_shared_alloc, nmemb, size)

@memalignreplacer@
expression size;
expression align;
@@
- flexos_memalign_whitelist(align, size, ...)
+ uk_memalign(flexos_shared_alloc, align, size)

@pallocreplacer@
expression pages;
@@
- flexos_palloc_whitelist(pages, ...)
+ uk_palloc(flexos_shared_alloc, pages)

@pfreereplacer@
expression var;
@@
- flexos_pfree_whitelist(var, ...)
+ uk_pfree(flexos_shared_alloc, var)

@freereplacer@
expression var;
@@
- flexos_free_whitelist(var, ...)
+ uk_free(flexos_shared_alloc, var)
