#ifndef COSMIC_RCPP_COMPAT_H
#define COSMIC_RCPP_COMPAT_H

#include <Rinternals.h>

#ifdef __cplusplus
extern "C" {
#endif

/*
 * R-devel (toward R 4.6) stopped declaring the non-API variable
 * R_NamespaceRegistry in installed headers. Older Rcpp releases still refer to
 * it from Function.h. Declare it here so packages can compile on builders that
 * have the new R headers but not yet the corresponding Rcpp hotfix.
 */
extern SEXP R_NamespaceRegistry;

#ifdef __cplusplus
}
#endif

#endif
