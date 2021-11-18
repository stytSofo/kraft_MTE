@rmattrsarray@
type T;
identifier p;
expression size;
@@
T p[size]
- __attribute__((flexos_whitelist))
;

@rmattrsarray_init@
type T;
identifier p;
expression size;
@@
T p[size]
- __attribute__((flexos_whitelist))
= ...
;

@rmattrsvar@
type T;
identifier p;
@@
T p
- __attribute__((flexos_whitelist))
;

@rmattrsvar_init@
type T;
identifier p;
@@
T p
- __attribute__((flexos_whitelist))
= ...
;
