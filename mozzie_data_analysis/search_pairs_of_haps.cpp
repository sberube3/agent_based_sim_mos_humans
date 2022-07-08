#include <Rcpp.h>
#include <iostream>
#include <vector>
#include <algorithm.h>

using namespace Rcpp;



// [[Rcpp::export]]
NumericVector find_pairs_of_haps(CharacterVector haplotypes,List human_infections, int n_infec, int nhaps) {
  NumericMatrix prop_matrix(nhaps, nhaps);
  NumericVector has_hap_pair;
for(int i=0; i<nhaps; i++){
  for(int j =0; j<nhaps; j++){
    String hap_i = haplotypes[i];
    String hap_j = haplotypes[j];
    has_hap_pair = rep(0,n_infec);
    for(int k=0; k<n_infec; k++){
      CharacterVector infec= human_infections[k];
      
      if(std::find() ){
        has_hap_pair[k] = 1;
      }
    }
    prop_matrix(i,j)= sum(has_hap_pair);
  }
}

return(prop_matrix);

}

