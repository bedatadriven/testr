// Generated by using Rcpp::compileAttributes() -> do not edit by hand
// Generator token: 10BE3573-1514-4C36-9D1C-5A225CD40393

#include "../inst/include/testr.h"
#include <Rcpp.h>

using namespace Rcpp;

// WriteCapInfo_cpp
void WriteCapInfo_cpp(CharacterVector fname, SEXP args_env);
RcppExport SEXP testr_WriteCapInfo_cpp(SEXP fnameSEXP, SEXP args_envSEXP) {
BEGIN_RCPP
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< CharacterVector >::type fname(fnameSEXP);
    Rcpp::traits::input_parameter< SEXP >::type args_env(args_envSEXP);
    WriteCapInfo_cpp(fname, args_env);
    return R_NilValue;
END_RCPP
}
