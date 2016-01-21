// This file was generated by Rcpp::compileAttributes
// Generator token: 10BE3573-1514-4C36-9D1C-5A225CD40393

#include <Rcpp.h>

using namespace Rcpp;

// Findweights
Rcpp::NumericVector Findweights(NumericVector ONv, NumericVector NNv, NumericVector WEv, int nobs, int nnew, int ntree, double thres, NumericVector counti, int normalise);
RcppExport SEXP quantregRanger_Findweights(SEXP ONvSEXP, SEXP NNvSEXP, SEXP WEvSEXP, SEXP nobsSEXP, SEXP nnewSEXP, SEXP ntreeSEXP, SEXP thresSEXP, SEXP countiSEXP, SEXP normaliseSEXP) {
BEGIN_RCPP
    Rcpp::RObject __result;
    Rcpp::RNGScope __rngScope;
    Rcpp::traits::input_parameter< NumericVector >::type ONv(ONvSEXP);
    Rcpp::traits::input_parameter< NumericVector >::type NNv(NNvSEXP);
    Rcpp::traits::input_parameter< NumericVector >::type WEv(WEvSEXP);
    Rcpp::traits::input_parameter< int >::type nobs(nobsSEXP);
    Rcpp::traits::input_parameter< int >::type nnew(nnewSEXP);
    Rcpp::traits::input_parameter< int >::type ntree(ntreeSEXP);
    Rcpp::traits::input_parameter< double >::type thres(thresSEXP);
    Rcpp::traits::input_parameter< NumericVector >::type counti(countiSEXP);
    Rcpp::traits::input_parameter< int >::type normalise(normaliseSEXP);
    __result = Rcpp::wrap(Findweights(ONv, NNv, WEv, nobs, nnew, ntree, thres, counti, normalise));
    return __result;
END_RCPP
}
// Findweightsfast
Rcpp::NumericVector Findweightsfast(NumericVector OrdNv, NumericVector NNv, NumericVector filterednodes, IntegerVector index, IntegerVector newindex, NumericVector WEv, int nobs, int nnew, int ntree, double thres, int l);
RcppExport SEXP quantregRanger_Findweightsfast(SEXP OrdNvSEXP, SEXP NNvSEXP, SEXP filterednodesSEXP, SEXP indexSEXP, SEXP newindexSEXP, SEXP WEvSEXP, SEXP nobsSEXP, SEXP nnewSEXP, SEXP ntreeSEXP, SEXP thresSEXP, SEXP lSEXP) {
BEGIN_RCPP
    Rcpp::RObject __result;
    Rcpp::RNGScope __rngScope;
    Rcpp::traits::input_parameter< NumericVector >::type OrdNv(OrdNvSEXP);
    Rcpp::traits::input_parameter< NumericVector >::type NNv(NNvSEXP);
    Rcpp::traits::input_parameter< NumericVector >::type filterednodes(filterednodesSEXP);
    Rcpp::traits::input_parameter< IntegerVector >::type index(indexSEXP);
    Rcpp::traits::input_parameter< IntegerVector >::type newindex(newindexSEXP);
    Rcpp::traits::input_parameter< NumericVector >::type WEv(WEvSEXP);
    Rcpp::traits::input_parameter< int >::type nobs(nobsSEXP);
    Rcpp::traits::input_parameter< int >::type nnew(nnewSEXP);
    Rcpp::traits::input_parameter< int >::type ntree(ntreeSEXP);
    Rcpp::traits::input_parameter< double >::type thres(thresSEXP);
    Rcpp::traits::input_parameter< int >::type l(lSEXP);
    __result = Rcpp::wrap(Findweightsfast(OrdNv, NNv, filterednodes, index, newindex, WEv, nobs, nnew, ntree, thres, l));
    return __result;
END_RCPP
}
// Findweightsinbag
Rcpp::NumericVector Findweightsinbag(NumericVector ONv, IntegerVector inbag, NumericVector WEv, int nobs, int nnew, int ntree, double thres, NumericVector counti, int normalise);
RcppExport SEXP quantregRanger_Findweightsinbag(SEXP ONvSEXP, SEXP inbagSEXP, SEXP WEvSEXP, SEXP nobsSEXP, SEXP nnewSEXP, SEXP ntreeSEXP, SEXP thresSEXP, SEXP countiSEXP, SEXP normaliseSEXP) {
BEGIN_RCPP
    Rcpp::RObject __result;
    Rcpp::RNGScope __rngScope;
    Rcpp::traits::input_parameter< NumericVector >::type ONv(ONvSEXP);
    Rcpp::traits::input_parameter< IntegerVector >::type inbag(inbagSEXP);
    Rcpp::traits::input_parameter< NumericVector >::type WEv(WEvSEXP);
    Rcpp::traits::input_parameter< int >::type nobs(nobsSEXP);
    Rcpp::traits::input_parameter< int >::type nnew(nnewSEXP);
    Rcpp::traits::input_parameter< int >::type ntree(ntreeSEXP);
    Rcpp::traits::input_parameter< double >::type thres(thresSEXP);
    Rcpp::traits::input_parameter< NumericVector >::type counti(countiSEXP);
    Rcpp::traits::input_parameter< int >::type normalise(normaliseSEXP);
    __result = Rcpp::wrap(Findweightsinbag(ONv, inbag, WEv, nobs, nnew, ntree, thres, counti, normalise));
    return __result;
END_RCPP
}
// Findweightsinbagfast
Rcpp::NumericVector Findweightsinbagfast(NumericVector ONv, NumericVector OrdNv, NumericVector filterednodes, IntegerVector index, IntegerVector newindex, IntegerVector inbag, NumericVector WEv, int nobs, int ntree, double thres, int l);
RcppExport SEXP quantregRanger_Findweightsinbagfast(SEXP ONvSEXP, SEXP OrdNvSEXP, SEXP filterednodesSEXP, SEXP indexSEXP, SEXP newindexSEXP, SEXP inbagSEXP, SEXP WEvSEXP, SEXP nobsSEXP, SEXP ntreeSEXP, SEXP thresSEXP, SEXP lSEXP) {
BEGIN_RCPP
    Rcpp::RObject __result;
    Rcpp::RNGScope __rngScope;
    Rcpp::traits::input_parameter< NumericVector >::type ONv(ONvSEXP);
    Rcpp::traits::input_parameter< NumericVector >::type OrdNv(OrdNvSEXP);
    Rcpp::traits::input_parameter< NumericVector >::type filterednodes(filterednodesSEXP);
    Rcpp::traits::input_parameter< IntegerVector >::type index(indexSEXP);
    Rcpp::traits::input_parameter< IntegerVector >::type newindex(newindexSEXP);
    Rcpp::traits::input_parameter< IntegerVector >::type inbag(inbagSEXP);
    Rcpp::traits::input_parameter< NumericVector >::type WEv(WEvSEXP);
    Rcpp::traits::input_parameter< int >::type nobs(nobsSEXP);
    Rcpp::traits::input_parameter< int >::type ntree(ntreeSEXP);
    Rcpp::traits::input_parameter< double >::type thres(thresSEXP);
    Rcpp::traits::input_parameter< int >::type l(lSEXP);
    __result = Rcpp::wrap(Findweightsinbagfast(ONv, OrdNv, filterednodes, index, newindex, inbag, WEv, nobs, ntree, thres, l));
    return __result;
END_RCPP
}
// Findweightsinbagfastimp
Rcpp::List Findweightsinbagfastimp(NumericVector ONv, NumericVector ONvp, NumericVector OrdNv, NumericVector filterednodes, IntegerVector index, IntegerVector newindex, IntegerVector inbag, NumericVector WEv, NumericVector WEvp, int npred, int nobs, int ntree, double thres, int l, IntegerVector countbreak);
RcppExport SEXP quantregRanger_Findweightsinbagfastimp(SEXP ONvSEXP, SEXP ONvpSEXP, SEXP OrdNvSEXP, SEXP filterednodesSEXP, SEXP indexSEXP, SEXP newindexSEXP, SEXP inbagSEXP, SEXP WEvSEXP, SEXP WEvpSEXP, SEXP npredSEXP, SEXP nobsSEXP, SEXP ntreeSEXP, SEXP thresSEXP, SEXP lSEXP, SEXP countbreakSEXP) {
BEGIN_RCPP
    Rcpp::RObject __result;
    Rcpp::RNGScope __rngScope;
    Rcpp::traits::input_parameter< NumericVector >::type ONv(ONvSEXP);
    Rcpp::traits::input_parameter< NumericVector >::type ONvp(ONvpSEXP);
    Rcpp::traits::input_parameter< NumericVector >::type OrdNv(OrdNvSEXP);
    Rcpp::traits::input_parameter< NumericVector >::type filterednodes(filterednodesSEXP);
    Rcpp::traits::input_parameter< IntegerVector >::type index(indexSEXP);
    Rcpp::traits::input_parameter< IntegerVector >::type newindex(newindexSEXP);
    Rcpp::traits::input_parameter< IntegerVector >::type inbag(inbagSEXP);
    Rcpp::traits::input_parameter< NumericVector >::type WEv(WEvSEXP);
    Rcpp::traits::input_parameter< NumericVector >::type WEvp(WEvpSEXP);
    Rcpp::traits::input_parameter< int >::type npred(npredSEXP);
    Rcpp::traits::input_parameter< int >::type nobs(nobsSEXP);
    Rcpp::traits::input_parameter< int >::type ntree(ntreeSEXP);
    Rcpp::traits::input_parameter< double >::type thres(thresSEXP);
    Rcpp::traits::input_parameter< int >::type l(lSEXP);
    Rcpp::traits::input_parameter< IntegerVector >::type countbreak(countbreakSEXP);
    __result = Rcpp::wrap(Findweightsinbagfastimp(ONv, ONvp, OrdNv, filterednodes, index, newindex, inbag, WEv, WEvp, npred, nobs, ntree, thres, l, countbreak));
    return __result;
END_RCPP
}
// test
Rcpp::List test(NumericVector ONv, NumericVector NNv, int ntree);
RcppExport SEXP quantregRanger_test(SEXP ONvSEXP, SEXP NNvSEXP, SEXP ntreeSEXP) {
BEGIN_RCPP
    Rcpp::RObject __result;
    Rcpp::RNGScope __rngScope;
    Rcpp::traits::input_parameter< NumericVector >::type ONv(ONvSEXP);
    Rcpp::traits::input_parameter< NumericVector >::type NNv(NNvSEXP);
    Rcpp::traits::input_parameter< int >::type ntree(ntreeSEXP);
    __result = Rcpp::wrap(test(ONv, NNv, ntree));
    return __result;
END_RCPP
}
