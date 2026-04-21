#ifndef COSMIC_RCPP_COMPAT_H
#define COSMIC_RCPP_COMPAT_H

/*
 * Avoid including Rinternals.h here: its macros (e.g. length, isNull) can
 * interfere with C++ standard headers and Rcpp when this file is force-included
 * via compiler flags. A forward declaration is enough for the missing symbol.
 */
struct SEXPREC;
typedef SEXPREC* SEXP;

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
