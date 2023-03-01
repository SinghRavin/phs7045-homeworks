#include<Rcpp.h>

using namespace Rcpp;

// [[Rcpp::export]]
List psmatch(
    NumericVector pscores,
    LogicalVector is_treated
)
{
  int n = pscores.size();
  IntegerVector indicies(n);
  NumericVector values(n);
  
  for (int i = 0; i < n; ++i) {
    int best_neigh = 0;
    double best_dist = std::numeric_limits< double >::max();
    for (int j = 0; j < n; ++j){
      
      if (j==i)
        continue;
      
      if (is_treated[j]==is_treated[i]){
        continue;
      }
      
      double tmp_dist = abs(pscores[i]-pscores[j]);
      if (tmp_dist < best_dist){
        best_dist = tmp_dist;
        best_neigh = j;
      }
    }
    
    indicies[i]=best_neigh;
    values[i] = pscores[best_neigh];
    
  }
  
  return List::create(_["match_id"]=indicies+1, _["match_x"]=values);
  
}