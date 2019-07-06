//' Get the patterns impurity scores
//'
//' @param data data
//' @param classes classes
//' @param patternPool patterns to get the scores for

/*******************************************************************************
* The main function impurityScores() calculates impurity scores of the data
* given each pattern:
* 
* For metric = "cond.entropy":
*    cond_entr(Y|X) = - sum_{x,y} [p(x,y) (log p(x,y) / p(x))] 
*  or for just two classes:
*    cond_entr(Y|X) = - TP/n * log(TP/(TP+FP)) - FP/n * log(FP/(TP+FP)) - 
*                     - FN/n * log(FN/(FN+TN)) - TN/n * log(TN/(FN+TN))
* The lower the entropy the better the pattern is in separating the data.
* 
* For metric = "Gini" :
*    gini(Y|X) = sum_{i} p(x)[p(y|x)(1-p(y|x))]
*  or for just two classes:
*    gini(Y|X) = 2TP*FP/n(TP+FP) + 2FN*TN/n(FN+TN)
* The lower the gini index the better the pattern is in separating the data.
* 
* For metric = "Fscore" :
*    F_b(Y|X) = (1+b^2) * precision * recall / (b^2 * precision + recall)
* The higher the F-score the better the pattern is in separating the data.
******************************************************************************/

#include <Rcpp.h>
#include <math.h>
using namespace Rcpp;

unsigned long cNumObs;
double cNumObsRecip;
unsigned long cNumPat; 

/*******************************************************************************
 * Function to get the impurity measure of a split.
 ******************************************************************************/
double measure(std::vector<int>& cPrediction, 
               IntegerVector& rClasses, 
               String& rMetric) {
  double cTP=0, cTN=0, cFP=0, cFN=0, cOut=0;
  
  for (int j=0; j<cNumObs; j++) {
    if (cPrediction[j] == rClasses[j]) cPrediction[j] ? ++cTP : ++cTN;
    else cPrediction[j] ? ++cFP : ++cFN;
  }
  if (rMetric == "cond.entropy") {
    if (cTP) cOut -= log(cTP / (cTP+cFP)) * cTP;
    if (cFN) cOut -= log(cFN / (cFN+cTN)) * cFN;
    if (cFP) cOut -= log(cFP / (cTP+cFP)) * cFP;
    if (cTN) cOut -= log(cTN / (cFN+cTN)) * cTN;
  } else if (rMetric == "Gini") {
    if (cTP+cFP) cOut += (cTP*cFP) / (cTP+cFP);
    if (cTN+cFN) cOut += (cFN*cTN) / (cTN+cFN);
  } else if (rMetric == "Fscore") {
    cOut = - cTP / (0.1 * (cTP + cFN) + (cTP + cFP)) ;
  } else {
    cOut = - cTP / (pow(cTP+cFN, 0.1) * pow(cTP+cFP, 0.9));
  }
  return cOut;
} /* measure() */
/******************************************************************************/


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
NumericVector impurityScores(DataFrame data, IntegerVector rClasses,
                             List patternPool, String& rMetric) {
  cNumObs = rClasses.size();
  if (cNumObs) cNumObsRecip = 1.0 / cNumObs;
  cNumPat = patternPool.size();
  std::vector<double> out(cNumPat); // filled with zeros
  Nodes tree;
  tree.N = cNumObs;
  tree.literal = "0";
  std::vector<int> ones (cNumObs, 1);
  tree.prediction = ones;
        
  /* building the tree */
  insertPat(tree, patternPool, data);
        
  /* for each pattern calculate its impurity score */
  for (int i=0; i<cNumPat; i++) {
    CharacterVector currPat = patternPool[i];
    std::vector<int> cPred = predict(tree, patternPool[i]);
    out[i] = measure(cPred, rClasses, rMetric);
  }

  return Rcpp::wrap(out);
} /* impurityScores() */
