@mallocreplacer@
expression size;
@@
- flexos_malloc_whitelist(size, ...)
+ malloc(size)

@reallocreplacer@
expression var;
expression size;
@@
- flexos_realloc_whitelist(var, size, ...)
+ realloc(var, size)

@callocreplacer@
expression size;
expression nmemb;
@@
- flexos_calloc_whitelist(nmemb, size, ...)
+ calloc(nmemb, size)

@memalignreplacer@
expression size;
expression align;
@@
- flexos_memalign_whitelist(align, size, ...)
+ memalign(align, size)

@pallocreplacer@
expression pages;
@@
- flexos_palloc_whitelist(pages, ...)
+ uk_palloc(uk_alloc_get_default(), pages)

@pfreereplacer@
expression var;
@@
- flexos_pfree_whitelist(var, ...)
+ uk_pfree(uk_alloc_get_default(), var)

@freereplacer@
expression var;
@@
- flexos_free_whitelist(var, ...)
+ free(var)
