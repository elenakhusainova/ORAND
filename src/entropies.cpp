//' Get the pattern conditional entropies
//'
//' @param data data
//' @param classes classes
//' @param patternPool patterns to get entropies for

/*******************************************************************************
 * The main function entropies() calculates conditional entropy of the data
 * given each pattern:
 *    cond_entr(Y|X) = - sum_{x,y} [p(x,y) (log p(x,y) / p(x))]
 * The lower the entropy the better the pattern is in separating the data.
 ******************************************************************************/

#include <Rcpp.h>
using namespace Rcpp;

struct Nodes {
  std::string literal;
  unsigned long N;                  // number of observations
  std::vector<int> prediction;      // prediction for the pattern constructed by
                                    // the literals along the path from the root
                                    // to the current node
  std::map<std::string, Nodes> children;
}; /* struct Nodes */

void insertPat(struct Nodes& tree,
               const List& patternPool,
               const DataFrame& data)
{
  const int nRows = data.nrows();

  std::vector<int> parentPred;
  struct Nodes *currTree;
  std::vector<int> pred;
  std::vector<std::string> varC;
  CharacterVector varR;;

  /* put each pattern from the root to its own leaf */
  for(const std::vector<std::string> &pat : patternPool) {
    currTree = &tree; // start from the top
    parentPred = currTree->prediction;
    for (auto &lit : pat) {
      /* if current node doesn't have the corresponding child, create new one */
      if (currTree->children.find(lit) == currTree->children.end()) {
        struct Nodes newNode;
        newNode.N = nRows;
        newNode.literal = lit;
        /* get the parent's prediction and update it with the new literal */
        pred = parentPred;
        std::size_t pos = lit.find("="); // literal has form "varName=varValue"
        std::string varName (lit.substr(0, pos)); // get all before "="
        std::string varValue (lit.substr(pos+1)); // get all after "="
        varR = data[varName];
        varC = Rcpp::as<std::vector<std::string>> (varR);
        for (int j = 0; j < nRows; ++j) pred[j] = pred[j] && (varC[j] == varValue);
        newNode.prediction = pred;
        currTree->children[lit] = newNode;
        currTree = &currTree->children[lit];
        parentPred = pred;
      }
      /* if current node has the corresponding child go down to it */
      else {
        currTree = &currTree->children[lit];
        parentPred = currTree->prediction;
      }
    }
  }
} /* insertPat() */

std::vector<int> predict(struct Nodes& tree,
                         std::vector<std::string> pattern)
{
  struct Nodes *currTree;
  currTree = &tree;
  std::vector<int> out;

  for (auto &lit : pattern) currTree = &currTree->children[lit];
  out = currTree->prediction;
  return out;
} /* predict() */


// [[Rcpp::export]]
NumericVector entropies(DataFrame data, IntegerVector classes,
                        List patternPool) {
  const  int nObs = classes.size();
  const int nPat = patternPool.size();
  NumericVector out(nPat); // filled with zeros
  Nodes tree;
  tree.N = nObs;
  tree.literal = "0";
  std::vector<int> ones (nObs, 1);
  tree.prediction = ones;

  /* building the tree */
  insertPat(tree, patternPool, data);

  /* for each pattern calculate its entropy */
  for (int i=0; i<nPat; i++) {
    CharacterVector currPat = patternPool[i];
    IntegerVector pred = Rcpp::wrap(predict(tree, patternPool[i]));
    int tp=0, tn=0, fp=0, fn=0;
    for (int j=0; j<nObs; j++) {
      if (pred[j] == classes[j]) pred[j] ? ++tp : ++tn;
      else pred[j] ? ++fp : ++fn;
    }
    if (tp) out[i] -= log(static_cast<double>(tp) / (tp+fp)) * tp  / nObs;
    if (fn) out[i] -= log(static_cast<double>(fn) / (fn+tn)) * fn  / nObs;
    if (fp) out[i] -= log(static_cast<double>(fp) / (tp+fp)) * fp  / nObs;
    if (tn) out[i] -= log(static_cast<double>(tn) / (fn+tn)) * fn  / nObs;
  }
  return out;
} /* entropies() */
