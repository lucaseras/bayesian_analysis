structure Bayesian =
struct

  exception Unimplemented

  (*  Naming conventions.  I found it useful to consistently use the same names
  *   for the same types of values:
  *   - c : category
  *   - d : doc
  *   - ccs : ctycounts
  *   - wcs : wordcounts
  *   - wps : wordprobs
  *   - keyws : string list (list of keywords)
  *)

  (*  These type declarations just let us use meaningful names for the types
  *  that we use here.
  *)

  type category = string
  type doc = string list

  (*  Think of a value wcs : wordcounts as a function from strings to ints.
  *   If wcs = [(s_0, i_0),...,(s_n, i_n)], then the value wcs on string s
  *   is computed as follows:
  *   - if s = s_j, then the value is i_j
  *   - if s <> s_j for any j, then the value is undefined.
  *   In the documentation below, we will write wcs(s) for the value of wcs
  *   on string s.
  *
  *   Think of each of the list types below in the same way.
  *)
  type wordcounts = (string*int) list

  (*  The type of functions from categories to wordcounts.
  *)
  type ctycounts = (category*wordcounts) list

  (*  A type of functions from strings to reals.
  *)
  type wordprobs = (string*real) list

  (*  A type of functions from categories to wordprobs.
  *)
  type ctyprobs = (category*wordprobs) list

  exception Failure


  (*  ****************************************
  *   Basic list processing functions.
  *   ****************************************
  *)

  (*  remove(xs, x) = ys, where ys is a copy of xs with all occurrences of x
  *  removed.
  *)
  fun remove (x : ''a) = List.filter(fn y => not(x = y))

  (*  removeDups xs = ys, where ys is a copy of xs with all duplicates removed.
  *)
  (*val removeDups = List.foldr (fn(x, y) => x :: remove x y) []*)
  fun removeDups (xs : ''a list) = List.foldr (fn(x, y) => x :: remove x y) [] xs
  
  (*  member(x) = fn([x_0,...,x_{n-1}]) = true,  x = x_i for some i
  *                                     = false, x <> x_i for all i.
  *)
  fun member (x : ''a) : (''a list -> bool) = List.exists (fn(y) => x = y)

  (*  ****************************************
  *   Category-count getter and updating functions.
  *   ****************************************
  *)

  (*  getCtyCounts(c) = fn(ccs) = ccs(c).
  *)
  fun getCtyCounts (c : category) : (getCtyCountsts -> wordcounts) =
    List.foldr (fn((cty, wcs), y) => if c = cty then wcs else y) []

  (*  updCtyCounts (c, wcs) = fn(ccs) = ccs', where:
  *   - ccs'(c) = wcs
  *   - ccs'(c') = ccs(c') if c' <> c.
  *   In other words, updCtyCounts(ccs, c, wcs) is just like ccs, except
  *   the value on category c is wcs.
  *)
  
  fun updCtyCounts (
      c : category, 
      wcs : wordcounts) : (ctycounts -> ctycounts) =
    List.map
    (fn((cty, wcs')) =>
      if c = cty then (cty, wcs) else (cty, wcs'))


  (*  ****************************************
  *   Word-count getter and updating functions.
  *   ****************************************
  *)

  (*  getWordCount(w) = fn(wcs) = wcs(w).
  *)
  fun getWordCount(w : string) : (wordcounts -> int) =
    fn(wcs) => 
    case List.find (fn((w',_)) => w' = w) wcs of
      NONE => raise Failure
    | SOME (w, n) => n


  (*  updWordCounts(w, n) = fn(wcs) = wcs', where
  *   - wcs'(w) = n
  *   - wcs'(w') = wcs(w') if w' <> w.
  *)
  fun updWordCounts(w : string, n : int) : (wordcounts -> wordcounts) =
    List.map(fn((w', n')) => if w' = w then (w',n) else (w',n'))

  (*  addWordCounts(wcs1) = fn(wcs2) = wcs, where wcs(w) = wcs1(w) + wcs2(w)
  *)
  fun addWordCounts(wcs1 : wordcounts) : (wordcounts -> wordcounts) =
    List.foldr(fn((w,n),y) => updWordCounts(w, (n + getWordCount w y)) y) wcs1

  (*  incrCount (w) = fn(wcs) = wcs', where:
  *   - wcs'(w) = 1 + wcs(w)
  *   - wcs'(w') = if w' <> w.
  *   In other words, incrCount keyws (w, wcs) is just like wcs, except the
  *   count for w is incremented.
  *)
  fun incrCount (w : string) : (wordcounts -> wordcounts) =
    List.map(fn((word, cnt)) => if w = word then (word, cnt+1) else (word, cnt))

  (*  initCounts = fn(keyws) = wcs, where wcs(w) = 0 for each w in keys.
  *)
  val initCounts : (string list -> wordcounts) =
    List.map(fn(x) => (x, 0)) 

(*  ****************************************
  *   The main word-counting functions.
  *   ****************************************
  *)

  (*  countDoc(keyws) = fn(d) = wcs, where for k in keyws, wcs(k) is the 
  *  number of occurrences of k in d.
  *)
  fun countDoc(keyws : string list) : (doc -> wordcounts) =
    List.foldr(fn(w, y) => 
      if member w keyws then incrCount(w) y else y) (initCounts keyws)


  (*  count (keyws, cds) = ccs, where ccs(c) is the word count for all documents
  *   in cds with category c.
  *)
  fun count (keyws : string list, cds : (category*doc) list) : ctycounts =
    let
    val initKCounts = initCounts keyws

    (*  getCtys = fn([(c_0, d_0),..., (c_{n-1}, d_{n-1})] = [c_0,...,c_{n-1}].
    *)
    val getCtys : ((category*doc) list -> category list) =
      List.map(fn((x, _)) => x)

    (*  makeInitCtyCounts = fn(ctys) = ccs, where ccs(c) = initKCounts for each
    *   c in ctys.
    *)
    val makeInitCtyCounts : (category list -> ctycounts) =
      List.map(fn(cty) => (cty, initKCounts))

    val initCtyCounts = makeInitCtyCounts(removeDups(getCtys(cds)))

    (*  countDocs = fn([(c_0,d_0),...,(c_{n-1}, d_{n-1})]) = ccs, 
    *  where domain(ccs) = the unique categories in cds (the argument 
    *  to count), ccs(c) = wcs,
    *  where wcs(w) = the number of occurrences of w in all documents d_i such
    *  that c_i = c.
    *)
    val countDocs : ((category*doc) list -> ctycounts) =
      (*y = ccs*)
      let
        fun countDocs'((c, d), y) =
          let 
            val wcs : wordcounts = countDoc(keyws) d
          in
            updCtyCounts(c, addWordCounts(getCtyCounts(c) y) wcs) y
          end
      in
        List.foldr(fn(x, y) => countDocs'(x, y)) initCtyCounts
      end
  in
    countDocs cds
  end


  (*  ****************************************
  *   wordprobs and ctyprobs utility functions.
  *   ****************************************
  *)
  
  (*  getProb (w) = fun(wps) = wps(w).
  *)
  fun getProb (w : string) : (wordprobs -> real) =
    fn(wps) =>
    case List.find(fn((word, p)) => word = w) wps of
      NONE => raise Failure
    | SOME (w, p) => p

  (*fun getProb (wps : wordprobs, w : string) : real =
    case wps of
         [] => raise Fail "getProb"
       | (word, p) :: wps' => if w = word then p else getProb(wps', w)*)

  (*  makeWordProbs wcs = wps, where wps(s) = wcs(s)/n and n is the sum
  *   of the counts of all the words in wcs.
  *)

  fun makeWordProbs (wcs : wordcounts) : wordprobs =
  let
    (*  countWords = fn([(w_0,n_0),...,(w_{k-1}, n_{k-1})]) 
    *  = n_0 + ... + n_{k-1}.
    *)
    val countWords : (wordcounts -> int) =
      List.foldr(fn((_, n), y) => n + y) 0

    (*  nWords = n_0 + ... + n_{k-1}, where 
    *     wcs = [(w_0,n_0),...,(w_{k-1}, n_{k-1})].
    *)
    val nWords = countWords wcs

    (*  countProbs = fn([(w_0, n_0),...,(w_{k-1},n_{k-1})]) =
    *     [(w_0,p_0),...,(w_{n-1}, p_{n-1})], where 
    *     p_i = max(1, n_i)/(n_0 + ... + n_{k-1}).
    *)

    val countProbs : (wordcounts -> wordprobs) =
      List.map(fn((w,n)) => (w, real(Int.max(1, n))/real(nWords)))

  in
    countProbs wcs
  end

  (*  makeCtyProbs ccs = cps, where cps(c) = makeWordProbs(ccs(c))
  *)
  val makeCtyProbs : (ctycounts -> ctyprobs) =
    List.map(fn((c, wcs)) => (c, makeWordProbs(wcs)))

  (*  computeLL (keyws, d, wps) = the log-likelihood of the document d
  *   being produced by the model wps.  See assignment description for details.
  *)
  fun computeLL (keyws : string list, d : doc, wps : wordprobs) : real =
    let
    val dCounts : wordcounts = countDoc keyws d

    (*  getCountsWProbs = fn([(w_0,n_0),...,(w_{k-1}, n_{k-1})]) =
    *     [(n_0, p_0),..., (n_{k-1}, p_{k-1})], where p_i = getProb(wps, w_i).
    *)
    val getCountsWProbs : (wordcounts -> (int*real) list) =
      List.map(fn((w,n)) => (n, getProb(w) wps))


    (*  countsWProbs = fn([(n_0, p_0),..., (n_{k-1}, p_{k-1})]), where
    *     keyws = [w_0,..., w_{k-1}],
    *     n_i   = the number of occurrences of w_i in d
    *     p_i   = getProb(wps, w_i)
    *)
    val countsWProbs = getCountsWProbs(dCounts)

    (*  getLL = fn([(n_0, p_0),..., (n_{k-1}, p_{k-1})]) =
    *     n_0*log(p_0) + ... + n_{k-1}*log(p_{k-1}), where log = log base 10.
    *)
    val getLL : ((int*real) list -> real) =
      List.foldr(fn((n,p),y) => real(n)*Math.log10(p) + y) 0.0

  in
    getLL(countsWProbs)
  end

  (*  makeClassifier (keyws, cds) = cl, where cl(d) = [...,(c, r),...],
  *   where the list ranges over pairs (c, r) such that c is a category
  *   in cds and r is the log-likelihood that the document d is produced
  *   by the model computed from c.
  *)
  fun makeClassifier
      (keyws : string list, cds : (category*doc) list) 
      : doc -> (category*real) list =
    let
      val cps = makeCtyProbs(count(keyws, cds))
    in
      fn(d) => List.map (fn ((c, wps)) => (c, computeLL(keyws, d, wps))) cps
    end

end
