
functor NaiveBayes (ClassSpec : sig
                                  structure Category : ORDERED
                                  val default_category : Category.t

                                  structure Dataset : MAP_REDUCE
                                end) : NAIVE_BAYES_CLASSIFIER =
struct

    type category = ClassSpec.Category.t

    type labeled_document = category * string Seq.seq
    type document = string Seq.seq

    structure Dataset = ClassSpec.Dataset

    (* TASK instantiate the ExtractCombine functor 3 times, and
            define CatDict and WordDict and CatWordDict
            to be the dictionary modules this produces
    *)
    structure CatEC : EXTRACT_COMBINE =
        ExtractCombine (struct
                              structure Key = ClassSpec.Category
                              structure MR = Dataset
                        end)

    structure WordEC : EXTRACT_COMBINE =
        ExtractCombine (struct
                              structure Key = StringLt
                              structure MR = Dataset
                        end)

    structure CatWordEC : EXTRACT_COMBINE =
        ExtractCombine (struct
                          structure Key = PairOrder (struct
                                                            structure O1 = ClassSpec.Category
                                                            structure O2 = StringLt
                                                     end)
                          structure MR = Dataset
                        end)

    structure CatDict = CatEC.D
    structure WordDict = WordEC.D
    structure CatWordDict = CatWordEC.D



    type statistics =
          int CatDict.dict     (* maps each category to number of documents with that category *)
        * int CatDict.dict     (* maps each category to number of words in documents with that category *)
        * int CatWordDict.dict (* maps each (cat,word) to frequency *)
        * category Seq.seq     (* list of categories (no duplicates) *)
        * int                  (* total number of documents *)
        * int                  (* total number of different words *)

    (* TASK *)
    fun gather (train : labeled_document Dataset.mapreducable) : statistics =
        let val CatDoc = CatEC.extractcombine (fn (cat, _) => Seq.singleton (cat, 1)) Int.+ train
            val CatWord = CatEC.extractcombine (fn (cat, stringseq) => Seq.singleton (cat, Seq.length stringseq)) Int.+ train
            val freqCatWord = CatWordEC.extractcombine (fn (cat, stringseq) => Seq.map (fn word => ((cat, word), 1)) stringseq) Int.+ train
            val CatSeq = Seq.map (fn (k,v) => k) (CatDict.toSeq CatDoc)
            val DocNum = Seq.mapreduce (fn (k,v) => v) 0 Int.+ (CatDict.toSeq CatDoc)
            val WordNum = WordDict.size
                          (WordEC.extractcombine (fn (cat, stringseq) =>
                                                      Seq.map (fn word => (word, 0)) stringseq)
                                                 (fn (n1, n2) => 0)
                                                 train)
        in
          (CatDoc, CatWord, freqCatWord, CatSeq, DocNum, WordNum)
        end
(*
  AnagramNum = Seq.map (fn (sw, w)=> w)
                (WordEC.extractcombine (fn (cat, stringseq) =>
                                          Seq.map (fn word => (sort(word), Seq.singleton word)) stringseq)
                                     (fn (n1, n2) => Seq.append)
                                     train))
 *)

    (* TASK *)
    fun possible_classifications
        ((num_docs_by_cat,
          num_words_by_cat,
          freqs,
          all_categories,
          total_num_docs,
          total_num_words) : statistics)
        (test_doc : document) : (category * real) Seq.seq =
        Seq.map (fn cat => (cat, Math.ln(real(CatDict.lookup' num_docs_by_cat cat) / real(total_num_docs))
                              + (Seq.reduce (fn (x,y) => x+y) 0.0
                                            (Seq.map (fn word => case CatWordDict.lookup freqs (cat, word) of
                                                                      NONE => Math.ln(1.0 / real(total_num_words))
                                                                    | SOME freq => Math.ln(real(freq) / real(CatDict.lookup' num_words_by_cat cat))
                                                     )
                                            test_doc)))
                )
                all_categories

    (* TASK *)
    fun classify (stats : statistics)
                 (test_doc : document) : (category * real) =
                 Seq.reduce (fn ((c1,r1), (c2,r2)) => case r1<=r2 of
                                                           true => (c2,r2)
                                                          | false => (c1,r1))
                            (ClassSpec.default_category, Real.negInf)
                            (possible_classifications stats test_doc)

    (* TASK *)
    fun train_classifier (train : labeled_document Dataset.mapreducable) : document -> (category * real) =
        classify (gather train)

end


(*
TestFile.number_correct (TestFile.open_file "data/RCV1.small_train.txt")
(TestFile.open_file "data/RCV1.small_test.txt");
small - (5,8) success rate = 5/8 = 62.5%

TestFile.number_correct (TestFile.open_file "data/RCV1.medium_train.txt")
(TestFile.open_file "data/RCV1.medium_test.txt");
medium - (680,808) success rate = 680/808 = 84.2%

TestFile.number_correct (TestFile.open_file "data/RCV1.big_train.txt")
(TestFile.open_file "data/RCV1.big_test.txt");
big - (70122,78899) success rate = 70122/78899 = 88.9%


mismatched size:
TestFile.number_correct (TestFile.open_file "data/RCV1.big_train.txt")
= (TestFile.open_file "data/RCV1.small_test.txt");



comment: 1) When the train and test size are relatively the same, the bigger size
         will result in a higher accuracy of predicting.
         2) When the training data is significantly larger than the test data,
         the prediction gets more accurate as well, since it means that there are
         more samples to refer.
*)
