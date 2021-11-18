@dss_array@
identifier p;
fresh identifier pm = "_dss_" ## p;
type T;
function func;
expression size;
@@
func (...) {
... when exists
    when any
T p[size]
- __attribute__((flexos_whitelist))
;
+ T *pm = (T *) (((uintptr_t) &p) + STACK_SIZE);
<...
- p
+ pm
...>
}

@dss_primitive_init@
identifier p;
fresh identifier pm = "_dss_" ## p;
type T;
function func;
expression E;
@@
func (...) {
... when exists
    when any
T p
- __attribute__((flexos_whitelist)) = E
;
+ T *pm = (T *) (((uintptr_t) &p) + STACK_SIZE);
+ *(pm) = E;
<...
- p
+ (*pm)
...>
}

@dss_primitive_noinit@
identifier p;
fresh identifier pm = "_dss_" ## p;
type T;
function func;
@@
func (...) {
... when exists
    when any
T p
- __attribute__((flexos_whitelist))
;
+ T *pm = (T *) (((uintptr_t) &p) + STACK_SIZE);
<...
- p
+ (*pm)
...>
}
