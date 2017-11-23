#include <R.h>
#include <Rinternals.h>
#include <stdlib.h> // for NULL
#include <R_ext/Rdynload.h>

/* FIXME: 
   Check these declarations against the C/Fortran source code.
*/

/* .Call calls */
extern SEXP _packcircles_do_progressive_layout(SEXP);
extern SEXP _packcircles_doCirclePack(SEXP, SEXP);
extern SEXP _packcircles_iterate_layout(SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP);
extern SEXP _packcircles_select_non_overlapping(SEXP, SEXP, SEXP);

static const R_CallMethodDef CallEntries[] = {
    {"_packcircles_do_progressive_layout",  (DL_FUNC) &_packcircles_do_progressive_layout,  1},
    {"_packcircles_doCirclePack",           (DL_FUNC) &_packcircles_doCirclePack,           2},
    {"_packcircles_iterate_layout",         (DL_FUNC) &_packcircles_iterate_layout,         8},
    {"_packcircles_select_non_overlapping", (DL_FUNC) &_packcircles_select_non_overlapping, 3},
    {NULL, NULL, 0}
};

void R_init_packcircles(DllInfo *dll)
{
    R_registerRoutines(dll, NULL, CallEntries, NULL, NULL);
    R_useDynamicSymbols(dll, FALSE);
}
