#include <R_ext/RS.h>
#include <stdlib.h> // for NULL
#include <R_ext/Rdynload.h>

/* FIXME: 
   Check these declarations against the C/Fortran source code.
*/

/* .Fortran calls */
extern void F77_NAME(binsrt)(void *, void *, void *, void *, void *,
                             void *, void *, void *, void *, void *);
extern void F77_NAME(intri)(void *, void *, void *, void *, void *,
                            void *);
extern void F77_NAME(master)(void *, void *, void *, void *, void *,
                             void *, void *, void *, void *, void *,
                             void *, void *, void *, void *, void *,
                             void *, void *);
extern void F77_NAME(mnnd)(void *, void *, void *, void *, void *);

static const R_FortranMethodDef FortranEntries[] = {
    {"binsrt", (DL_FUNC) &F77_NAME(binsrt), 10},
    {"intri",  (DL_FUNC) &F77_NAME(intri),   6},
    {"master", (DL_FUNC) &F77_NAME(master), 17},
    {"mnnd",   (DL_FUNC) &F77_NAME(mnnd),    5},
    {NULL, NULL, 0}
};

void R_init_deldir(DllInfo *dll)
{
    R_registerRoutines(dll, NULL, NULL, FortranEntries, NULL);
    R_useDynamicSymbols(dll, FALSE);
}

