
functor ExtractCombine (A : sig
                                structure Key : ORDERED
                                structure MR : MAP_REDUCE
                            end) : EXTRACT_COMBINE =
struct

    structure MR = A.MR

    structure D = Dict(A.Key)

    fun extractcombine (extract: ('a -> (D.Key.t * 'v) Seq.seq))
                       (combine: ('v * 'v -> 'v))
                       (data: 'a MR.mapreducable)
                       : 'v D.dict =
        MR.mapreduce (fn x => Seq.mapreduce (fn (k,v) => D.insert D.empty (k,v))
                                            D.empty
                                            (D.merge combine)
                                            (extract x))
                     D.empty
                     (D.merge combine)
                     data

end
