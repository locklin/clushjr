;; Copyright (c) 2011 Scott Locklin/Lugos LLC
;; distributed under the zlib/libpng License


(ns idx
  (:import [org.jblas DoubleMatrix ComplexDoubleMatrix MatrixFunctions
            ConvertsToDoubleMatrix Eigen Solve DoubleFunction Singular Decompose
            Geometry ]))

(set! *warn-on-reflection* true)

;; sign
;; pow
;; sup
;; subset
(defn subset [^org.jblas.DoubleMatrix x ^:Integers ind])
;; into-idx
;; makes a list or double array into a DoubleMatrix
;; removerow
;; build-idx
;; range
;; irange
;; iexcluder (maybe)
;; rearrange (maybe not)


(defn zeros [a b]
  (DoubleMatrix/zeros a b))

(defn eye [n]
" requrns square eye matrix"
  (DoubleMatrix/eye n))

(defn rand-normal [n m]
  " returns an n by m matrix of normally distributed randoms
   optional arguments for mean and std?"
  (DoubleMatrix/randn n m))

(defn rand-flat [n m]
  "returns an n by m matrix of randoms between 0 and 1"
  (DoubleMatrix/rand n m))


(defn shape
  " returns shape of matrix, optional argument selects rows or cols
   repro of lush idx-shape"
  ([^org.jblas.DoubleMatrix x]
     [(.rows x) (.columns x)])
  ([^org.jblas.DoubleMatrix x rowcol]
     (cond (= rowcol 0) (.rows x)
           (= rowcol 1) (.columns x))))

(defn reshape [^org.jblas.DoubleMatrix x r c]
  " reshapes matrix"
  (.reshape x r c))

(defn issquare [^org.jblas.DoubleMatrix x]
  " squareness check"
  (.isSquare x))

(defn element-mul [^org.jblas.DoubleMatrix x ^org.jblas.DoubleMatrix y]
  " multiplies elements of x by y, returns a new matrix"
  (.mul x y))

(defn mul [^org.jblas.DoubleMatrix x ^org.jblas.DoubleMatrix y]
  (.mmul x y))

(defn rescale [^org.jblas.DoubleMatrix x ^org.jblas.DoubleMatrix y]
  "divides elements of x by y, returns a new matrix"
  (.div x y))

(defn take-real [^org.jblas.ComplexDoubleMatrix x]
  " reals of a complex matrix"
  (let [c (class x)]
  (cond (= c org.jblas.ComplexDoubleMatrix)
        (.getReal x)
        (= c org.jblas.DoubleMatrix)
        x)))


(defn take-imag [^org.jblas.ComplexDoubleMatrix x]
  " returns complex part of a complex matrix as a real matrix"
  (let [c (class x)]
  (cond (= c org.jblas.ComplexDoubleMatrix)
        (.imag x)
        (= c org.jblas.DoubleMatrix)
        (let [size (shape x)]
          (zeros (first size) (last size))))))


(defn add [^org.jblas.DoubleMatrix x ^org.jblas.DoubleMatrix y]
  "adds x to y, returns a new matrix"
  (.add x y))

(defn subtract [^org.jblas.DoubleMatrix x ^org.jblas.DoubleMatrix y]
  "subtracts y from x, returns a new matrix"
  (.add x (.neg y)))

(defn elt-max [^org.jblas.DoubleMatrix x]
  "largest element of x"
  (.max x))

(defn elt-min [^org.jblas.DoubleMatrix x]
  "smallest element of x"
  (.min x))

(defn ind-max [^org.jblas.DoubleMatrix x]
  " largest element index of x"
  (.argmax x))

(defn ind-min [^org.jblas.DoubleMatrix x]
  " smallest element index of x"
  (.argmin x))

(defn col-ind-max [^org.jblas.DoubleMatrix x]
  " largest element index of x  by columns"
  (.columnArgmaxs x))

(defn col-ind-min [^org.jblas.DoubleMatrix x]
  "smallest element index of x by columns"
  (.columnArgmins x))

(defn col-elt-max [^org.jblas.DoubleMatrix x]
  "largest element values of x  by columns"
  (.columnMaxs x))

(defn col-elt-min [^org.jblas.DoubleMatrix x]
  "smallest element values of x by columns"
  (.columnMins x))

(defn sum [^org.jblas.DoubleMatrix x]
  (.sum x))


(defn fill [^org.jblas.DoubleMatrix x ^Double v]
  " fills matrix x with value v"
  (.fill x v))

(defn ones [n m]
  "fills a n by m matrix with 1's"
  (let [out (zeros n m)]
    (fill out 1)))



(defn mat-transpose [^org.jblas.DoubleMatrix x]
  (.transpose x))



(defn dotc [^org.jblas.DoubleMatrix x ^Double y]
  " should be .mul, but doesn't work for some reason"
  (.mul x y))

(defn copy-matrix [^org.jblas.DoubleMatrix x]
  (.dup x))

(defn solve [^org.jblas.DoubleMatrix a ^org.jblas.DoubleMatrix b]
  "finds X for A*X = B"
  (Solve/solve a b))
;; also solveSymmetric for symmetric A
;; solvePositive for positive definite A


(defn abs [^org.jblas.DoubleMatrix a]
  (MatrixFunctions/abs a))



(defn wexp [^org.jblas.DoubleMatrix a]
  (MatrixFunctions/exp a))

(defn fracpos [^org.jblas.DoubleMatrix x]
    " what fraction are positive"
  (let [len (.length x)]
    (/ (sum (.gt x (double 0))) len)))


(defn fracneg [^org.jblas.DoubleMatrix x]
  "what fraction are negative"
  (let [len (.length x)]
    (/ (sum (.lt x (double 0))) len)))


(defn cumsum [^org.jblas.DoubleMatrix x]
  " cumulative sum by rows"
  (.cumulativeSum x))


(defn cholesky [a]
  "cholesky decomp of a"
  (Decompose/cholesky a))
;; missing in OS-X blas

(defn ludecomp [a]
  " lu decomposition of a"
  (Decompose/lu a))
;; missing in OS-X blas
;; returns lu class; change this to a list of three matrices, l, p (permutation) and u

(defn concat-horiz [x y]
  "concats matrices horizontally"
  (DoubleMatrix/concatHorizontally x y))

(defn concat-vert [x y]
  " concats matrices vertically"
  (DoubleMatrix/concatVertically x y))


(defn eigenvalues
  " returns complex matrix of eigenvalues and eigenvectors of x
   add an optional arg to take reals"
  ([^org.jblas.DoubleMatrix x]
     (if (issquare x)
       (Eigen/eigenvalues x)))
  ([^org.jblas.DoubleMatrix x nocomp]
     (if (issquare x)
       (take-real (Eigen/eigenvalues x)))))


(defn select [^org.jblas.DoubleMatrix x a b]
  " clone of idx-select in Lush; 0 gets columns 1 gets rows
   (select <mx> <rowcol> <num>)"
  (cond (= a 1) (.getColumn x b)
        (= a 0) (.getRow x b)))


(defn narrow [^org.jblas.DoubleMatrix x n s o]
"Make a clone of x and reduce its size in the n -th dimension to s elements, offset by o"
(let [nrow (shape x 0)
      ncol (shape x 1)]
  (cond
   (= n 1) (.getRange x 0 nrow o s)
   (= n 0) (.getRange x o s 0 ncol))))


(defn save-mx [^org.jblas.DoubleMatrix x ^String s]
  "saves a matrix to <filename>"
  (.save x s))

(defn load-mx [^org.jblas.DoubleMatrix x ^String s]
  "loads a matrix from <filename>"
  (.load x s))

(defn load-csv [^String s]
  (DoubleMatrix/loadCSVFile s))

(defn load-ascii [^String s]
  (DoubleMatrix/loadAsciiFile s))