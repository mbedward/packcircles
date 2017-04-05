#include <R.h>
#include <Rinternals.h>
#include <stdlib.h> // for NULL
#include <R_ext/Rdynload.h>

/* .Call calls */
extern SEXP packcircles_do_progressive_layout(SEXP);
extern SEXP packcircles_doCirclePack(SEXP, SEXP);
extern SEXP packcircles_iterate_layout(SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP);

static const R_CallMethodDef CallEntries[] = {
    {"packcircles_do_progressive_layout", (DL_FUNC) &packcircles_do_progressive_layout, 1},
    {"packcircles_doCirclePack",          (DL_FUNC) &packcircles_doCirclePack,          2},
    {"packcircles_iterate_layout",        (DL_FUNC) &packcircles_iterate_layout,        8},
    {NULL, NULL, 0}
};

void R_init_packcircles(DllInfo *dll)
{
    R_registerRoutines(dll, NULL, CallEntries, NULL, NULL);
    R_useDynamicSymbols(dll, FALSE);
}
