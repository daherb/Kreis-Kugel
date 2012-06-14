(* fun samedist(distlist,delta,modulo)=
  let val (fst::sec::rst)=distlist 
    fun samedist_aux([lst],delta,modulo,first)=
      Int.abs(lst mod modulo-first mod modulo)<delta
    | samedist_aux(fst::sec::rst,delta,modulo,first)=
      Int.abs(fst mod modulo-sec mod modulo)<=delta andalso samedist_aux(sec::rst,delta,modulo,first)
  in
    Int.abs(fst mod modulo-sec mod modulo)<=delta andalso samedist_aux(rst,delta,modulo,fst)
  end
*)
fun equal(fst::sec::rst,max)=
  let 
    fun eq_aux([],_)=(0,true)
    | eq_aux(lst::[],max)=(Int.abs(lst-max),true)
    | eq_aux(fst::sec::rst,max)=
      let 
        val (dist,ot)=eq_aux(sec::rst,max)
        val ndist=Int.abs(fst-sec)
      in (Int.min(dist,ndist),ot andalso dist=ndist)
      end
  in 
    eq_aux(fst::sec::rst,max)
  end
| equal([],max)=(0,true)
| equal(lst::[],max)=(Int.min(lst,Int.abs(max-lst)),lst=max div 2)

fun nextgen(oldgen,max,dmax,seed)=
  let
    fun ngen_aux(fst::rst,max,dmax,s)=((fst+Random.randRange(0,dmax) s) mod max)::ngen_aux(rst,max,dmax,s)
    | ngen_aux([],max,dmax,s)=[]
  in
    ngen_aux(oldgen,max,dmax,seed)
  end

fun intListToString(lst)=
  let
    fun aux1(fst::rst)=","^Int.toString(fst)^aux1(rst)
    | aux1([])=""
    fun aux(fst::rst)=Int.toString(fst)^aux1(rst)
    | aux([])=""
  in
    "["^aux(lst)^"]"
  end

local fun ins (n, []) = [n]
| ins (n, ns as h::t) = if (n<h) then n::ns else h::(ins (n, t))
in 
  val insertionSort = List.foldr ins []
end

fun doit(firstgen,max,dmax,d,seed)=
  let
    val (dist,t)=equal(firstgen,max)
  in
    TextIO.print ("gen "^intListToString(firstgen)^" - dist "^Int.toString(dist)^" - dmax "^Int.toString(dmax)^" - "^Int.toString(d)^"\n");
    if t orelse d<=2 then firstgen
    else
      let 
        val ngen=insertionSort(nextgen(firstgen,max,dmax,seed))
        val (ndist,nt)=equal(ngen,max)
      in
        if nt then ngen
        else
          if ndist>dist then doit(ngen,max,dmax-d,d div 2,seed)
          else doit (firstgen,max,dmax,d,seed)
      end
  end

fun doitnow()=
let
  val seed=Random.rand(300,Int.fromLarge(Time.toSeconds(Timer.checkRealTimer(Timer.totalRealTimer()))))
in
  doit([0,0,0,0],360,300,100,seed)
end
