//package dhg.ccg.parse.pcfg
//
//import org.junit.Test
//import org.junit.Assert._
//import dhg.ccg.parse._
//import dhg.ccg.parse.pcfg.mcmc._
//import dhg.ccg.prob._
//import dhg.ccg.cat._
//import dhg.ccg.tagdict._
//import dhg.util._
//import dhg.util._
//import dhg.ccg.util._
//import dhg.util._
//import dhg.util._
//import scala.util.Random
//
//class SupPcfgTrainerITests {
//
//  val A: Cat = cat"A"
//  val B: Cat = cat"B"
//  val C: Cat = cat"C"
//  val D: Cat = cat"D"
//  val E: Cat = cat"E"
//  val F: Cat = cat"F"
//  //    val G: Cat = cat"G"
//  //  val H: Cat = cat"H"
//  val Z: Cat = cat"Z"
//
//  @Test
//  def i_test_UnsmoothedSupPcfgTrainer_train {
//    throw new NotImplementedError("Test not written")
//  }
//
//  @Test
//  def i_test_AlphaBetaSupPcfgTrainer {
//    type Word = String
//
//    val alphaRoot = LogDouble(2.1)
//    val alphaBiny = LogDouble(2.3)
//    val alphaUnry = LogDouble(2.7)
//    val alphaTerm = LogDouble(2.9)
//    val alphaLambda = LogDouble(30.3)
//
//    val catIndexer = SimpleIndexer(Vector(A, B, C, D, E, F, Z))
//    val wordIndexer = SimpleIndexer("a1,a2,b1,c1,c2,c3,d1,d2,e1,z1".lsplit(","))
//    val numCats = catIndexer.size
//    val numWords = wordIndexer.size
//
//    val s1: CcgTree = CcgLeaf(A, "a1")
//    val s2: CcgTree = CcgLeaf(B, "b1")
//
//    val priorRootDistO: Map[Cat, LogDouble] = Map(
//      A -> 0.91,
//      B -> 0.92,
//      C -> 0.93,
//      D -> 0.94,
//      E -> 0.95,
//      F -> 0.96)
//      .mapVals(LogDouble(_))
//
//    val priorBinyDistO: Vector[(Cat, (BinaryProd, LogDouble))] = Vector(
//      (A, BinaryProd(B, C) -> 0.11),
//      (A, BinaryProd(E, D) -> 0.12),
//      (A, BinaryProd(E, F) -> 0.13),
//
//      (C, BinaryProd(A, D) -> 0.14),
//      (C, BinaryProd(D, E) -> 0.15),
//      (C, BinaryProd(D, F) -> 0.16),
//      (C, BinaryProd(E, D) -> 0.17),
//      (C, BinaryProd(E, F) -> 0.18),
//
//      (D, BinaryProd(B, C) -> 0.19),
//
//      (E, BinaryProd(D, F) -> 0.21),
//      (E, BinaryProd(B, C) -> 0.22))
//      .map { case (a, (b, c)) => (a, (b, LogDouble(c))) }
//
//    val priorUnryDistO: Vector[(Cat, (UnaryProd, LogDouble))] = Vector(
//      (A, UnaryProd(B) -> 0.23),
//      (A, UnaryProd(D) -> 0.24),
//      (A, UnaryProd(E) -> 0.25),
//      //(A, UnaryProd(Z) -> 0.41),
//
//      (C, UnaryProd(A) -> 0.26),
//      (C, UnaryProd(D) -> 0.27),
//      (C, UnaryProd(E) -> 0.28),
//
//      (D, UnaryProd(B) -> 0.42),
//
//      (E, UnaryProd(B) -> 0.43),
//      (E, UnaryProd(D) -> 0.44) // ,
//      //(Z, UnaryProd(Z) -> 0.45)
//      )
//      .map { case (a, (b, c)) => (a, (b, LogDouble(c))) }
//
//    val priorTermDistO: Vector[(Cat, (TermProd, LogDouble))] = Vector(
//      (A, TermProd("a1") -> 0.31),
//      (A, TermProd("a2") -> 0.32),
//      //(A, TermProd("z") -> 0.46),
//
//      (C, TermProd("c1") -> 0.33),
//      (C, TermProd("c2") -> 0.34),
//      (C, TermProd("c3") -> 0.35),
//
//      (D, TermProd("d2") -> 0.36),
//
//      (E, TermProd("e1") -> 0.37) //,
//      //(Z, TermProd("z") -> 0.47)
//      )
//      .map { case (a, (b, c)) => (a, (b, LogDouble(c))) }
//
//    val knownRoots: Array[Int] = priorRootDistO.keys.map(catIndexer).toArray.sorted
//    val knownBinys: Array[IndirectSparseVec[Array[Int]]] = DenseVec(priorBinyDistO.groupByKey.map { case (t, prods) => catIndexer(t) -> IndirectSparseVec(prods.collect { case (BinaryProd(u, v), _) => (catIndexer(u), catIndexer(v)) }.groupByKey.mapVals(_.toArray.sorted), numCats) }, numCats).values
//    val knownUnrys: IndirectSparseVec[Array[Int]] = IndirectSparseVec(priorUnryDistO.groupByKey.map { case (t, prods) => catIndexer(t) -> prods.collect { case (UnaryProd(u), _) => catIndexer(u) }.toArray.sorted }, numCats)
//    val knownTerms: Array[Array[Int]] = DenseVec(priorTermDistO.groupByKey.map { case (t, prods) => catIndexer(t) -> prods.collect { case (TermProd(w), _) => wordIndexer(w) }.toArray.sorted }, numCats).values
//
//    val priorRootDist: IndirectSparseVec[LogDouble] = IndirectSparseVec(priorRootDistO.mapKeys(catIndexer), numCats)
//    val priorBinyDist: Array[IndirectSparseVec[IndirectSparseVec[LogDouble]]] = DenseVec(priorBinyDistO.groupByKey.map { case (t, prods) => catIndexer(t) -> IndirectSparseVec(prods.toVector.collect { case (BinaryProd(u, v), p) => (catIndexer(u), (catIndexer(v), p)) }.groupByKey.mapVals(vps => IndirectSparseVec(vps.toMap, numCats)), numCats) }, numCats).values
//    val priorUnryDist: IndirectSparseVec[IndirectSparseVec[LogDouble]] = IndirectSparseVec(priorUnryDistO.groupByKey.map { case (t, prods) => catIndexer(t) -> IndirectSparseVec(prods.collect { case (UnaryProd(u), p) => (catIndexer(u), p) }, numCats) }, numCats)
//    val priorTermDist: Array[IndirectSparseVec[LogDouble]] = DenseVec(priorTermDistO.groupByKey.map { case (t, prods) => catIndexer(t) -> IndirectSparseVec(prods.collect { case (TermProd(w), p) => (wordIndexer(w), p) }, numWords) }, numCats).values
//
//    val mockResultingParser: PcfgParserI = new PcfgParserI(
//      rootDist = IndirectSparseVec.empty[LogDouble](0), //                         t -> p
//      binyDist = Array[IndirectSparseVec[IndirectSparseVec[LogDouble]]](), //       t -> u -> v -> p
//      unryDist = IndirectSparseVec.empty[IndirectSparseVec[LogDouble]](0), //              t -> u -> p
//      termDist = Array[IndirectSparseVec[LogDouble]](), //                  t -> w -> p
//      mixDist = Array[(LogDouble, LogDouble, LogDouble)]())( //          t -> (bmix,umix,tmix)
//      catIndexer = SimpleIndexer[Cat](Vector()), wordIndexer = SimpleIndexer[String](Vector()))
//
//    val mockPcfgParserInstantiater: PcfgParserInstantiaterI = new PcfgParserInstantiaterI {
//      def apply(
//        rootDist: IndirectSparseVec[LogDouble], //                         t -> p
//        binyDist: Array[IndirectSparseVec[IndirectSparseVec[LogDouble]]], //       t -> u -> v -> p
//        unryDist: IndirectSparseVec[IndirectSparseVec[LogDouble]], //              t -> u -> p
//        termDist: Array[IndirectSparseVec[LogDouble]], //                  t -> w -> p
//        mixDist: Array[(LogDouble, LogDouble, LogDouble)]) //      t -> (bmix,umix,tmix)
//        (catIndexer: Indexer[Cat], wordIndexer: Indexer[String]) = {
//
//        /* ROOTS
//         * A: 11+21   + (2.1 * 0.91) = 12.911  / 119.781 = 0.2831083393860462
//         * B: 25      + (2.1 * 0.92) = 26.932  / 119.781 = 0.22484367303662514
//         * C: 14      + (2.1 * 0.93) = 15.953  / 119.781 = 0.13318472879672066
//         * D: 15+22   + (2.1 * 0.94) = 38.974  / 119.781 = 0.3253771466259256
//         * E: 0       + (2.1 * 0.95) =  1.995  / 119.781 = 0.016655396097878628
//         * F: 0       + (2.1 * 0.96) =  2.016  / 119.781 = 0.016830716056803665
//         *    -----            ----   -------
//         *    108     +  2.1 * 5.61 = 119.781
//         */
//        assertEquals(7, rootDist.length)
//        assertEquals(6, rootDist.activeCount)
//        assertEqualsLog(LogDouble((32 + (2.1 * 0.91)) / (108 + 2.1 * 5.61)), rootDist(catIndexer(A)), 1e-9)
//        assertEqualsLog(LogDouble((25 + (2.1 * 0.92)) / (108 + 2.1 * 5.61)), rootDist(catIndexer(B)), 1e-9)
//        assertEqualsLog(LogDouble((14 + (2.1 * 0.93)) / (108 + 2.1 * 5.61)), rootDist(catIndexer(C)), 1e-9)
//        assertEqualsLog(LogDouble((37 + (2.1 * 0.94)) / (108 + 2.1 * 5.61)), rootDist(catIndexer(D)), 1e-9)
//        assertEqualsLog(LogDouble((0 + (2.1 * 0.95)) / (108 + 2.1 * 5.61)), rootDist(catIndexer(E)), 1e-9)
//        assertEqualsLog(LogDouble((0 + (2.1 * 0.96)) / (108 + 2.1 * 5.61)), rootDist(catIndexer(F)), 1e-9)
//
//        /* 
//         * BINARY PRODS
//         */
//
//        assertEquals(7, binyDist.length)
//        assertNull(binyDist(catIndexer(Z)))
//
//        /* 
//         * A -> BC  45+11 + (2.3 * 0.11) =  56.253  / 82.828 = 
//         * A -> ED  0     + (2.3 * 0.12) =   0.276  / 82.828 =
//         * A -> EF  26    + (2.3 * 0.13) =  26.299  / 82.828 =
//         *          -----          ----     ------
//         *          82    + (2.3 * 0.36) =  82.828  / 82.828 =
//         */
//
//        assertEquals(7, binyDist(catIndexer(A)).length)
//        assertEquals(2, binyDist(catIndexer(A)).activeCount)
//        assertEquals(7, binyDist(catIndexer(A))(catIndexer(B)).length)
//        assertEquals(1, binyDist(catIndexer(A))(catIndexer(B)).activeCount)
//        assertEqualsLog(LogDouble((45 + 11 + (2.3 * 0.11)) / (82 + (2.3 * 0.36))), binyDist(catIndexer(A))(catIndexer(B))(catIndexer(C)), 1e-9)
//        assertEquals(7, binyDist(catIndexer(A))(catIndexer(B)).length)
//        assertEquals(2, binyDist(catIndexer(A))(catIndexer(E)).activeCount)
//        assertEqualsLog(LogDouble((0 + (2.3 * 0.12)) / (82 + (2.3 * 0.36))), binyDist(catIndexer(A))(catIndexer(E))(catIndexer(D)), 1e-9)
//        assertEqualsLog(LogDouble((26 + (2.3 * 0.13)) / (82 + (2.3 * 0.36))), binyDist(catIndexer(A))(catIndexer(E))(catIndexer(F)), 1e-9)
//
//        /* 
//         * UNARY PRODS
//         */
//
//        assertEquals(7, unryDist.length)
//        assertEquals(4, unryDist.activeCount)
//
//        /* 
//         * A -> B  14+88 + (2.7 * 0.23) =  102.621  / 169.944 = 0.603851857082333
//         * A -> D  0     + (2.7 * 0.24) =    0.648  / 169.944 = 0.0038130207597796926
//         * A -> E  66    + (2.7 * 0.25) =   66.675  / 169.944 = 0.3923351221578873
//         *         -----          ----     -------
//         *         168   + (2.7 * 0.72) =  169.944
//         */
//
//        assertEquals(7, unryDist(catIndexer(A)).length)
//        assertEquals(3, unryDist(catIndexer(A)).activeCount)
//        assertEqualsLog(LogDouble((14 + 88 + (2.7 * 0.23)) / (168 + (2.7 * 0.72))), unryDist(catIndexer(A))(catIndexer(B)), 1e-9)
//        assertEqualsLog(LogDouble((0 + (2.7 * 0.24)) / (168 + (2.7 * 0.72))), unryDist(catIndexer(A))(catIndexer(D)), 1e-9)
//        assertEqualsLog(LogDouble((66 + (2.7 * 0.25)) / (168 + (2.7 * 0.72))), unryDist(catIndexer(A))(catIndexer(E)), 1e-9)
//
//        /* 
//         * TERMINAL PRODS
//         */
//
//        assertEquals(7, termDist.length)
//        assertNull(termDist(catIndexer(Z)))
//
//        /* 
//         * A -> a1  17+62 + (2.9 * 0.31) =   79.899  / 116.827 = 0.6839086854922236
//         * A -> a2  36    + (2.9 * 0.32) =   36.928  / 116.827 = 0.31609131450777644
//         *          -----          ----     -------
//         *          115   + (2.9 * 0.63) =  116.827
//         */
//
//        assertEquals(10, termDist(catIndexer(A)).length)
//        assertEquals(2, termDist(catIndexer(A)).activeCount)
//        assertEqualsLog(LogDouble((17 + 62 + (2.9 * 0.31)) / (115 + (2.9 * 0.63))), termDist(catIndexer(A))(wordIndexer("a1")), 1e-9)
//        assertEqualsLog(LogDouble((36 + (2.9 * 0.32)) / (115 + (2.9 * 0.63))), termDist(catIndexer(A))(wordIndexer("a2")), 1e-9)
//
//        /* 
//         * PROD MIX
//         */
//
//        assertEquals(7, mixDist.length)
//
//        /*
//         * A -> BC  45+11  
//         * A -> ED  0      
//         * A -> EF  26     
//         *          -----        
//         *          82    + 30.3*0.5 =  97.15 / 398.33 = 0.2438932543368564
//         * 
//         * A -> B   14+88 
//         * A -> D   0     
//         * A -> E   66    
//         *          -----           
//         *          168   + 30.3*0.4 = 180.12 / 398.33 = 0.45218788441744284
//         * 
//         * A -> a1  17+62 
//         * A -> a2  36    
//         *          -----          
//         *          115   + 30.3*0.2 = 121.06 / 398.33 = 0.30391886124570083
//         */
//
//        assertEqualsLog(LogDouble((82 + 30.3 * 0.5) / (365 + 30.3 * 1.1)), mixDist(catIndexer(A))._1, 1e-9)
//        assertEqualsLog(LogDouble((168 + 30.3 * 0.4) / (365 + 30.3 * 1.1)), mixDist(catIndexer(A))._2, 1e-9)
//        assertEqualsLog(LogDouble((115 + 30.3 * 0.2) / (365 + 30.3 * 1.1)), mixDist(catIndexer(A))._3, 1e-9)
//
//        /*
//         * 0 + 30.3*0.5 = 15.15 / 33.33 = 
//         * 0 + 30.3*0.4 = 12.12 / 33.33 = 
//         * 0 + 30.3*0.2 =  6.06 / 33.33 = 
//         *     --------
//         *     30.3*1.1 
//         */
//
//        assertEqualsLog(LogDouble((30.3 * 0.5) / (30.3 * 1.1)), mixDist(catIndexer(Z))._1, 1e-9)
//        assertEqualsLog(LogDouble((30.3 * 0.4) / (30.3 * 1.1)), mixDist(catIndexer(Z))._2, 1e-9)
//        assertEqualsLog(LogDouble((30.3 * 0.2) / (30.3 * 1.1)), mixDist(catIndexer(Z))._3, 1e-9)
//
//        mockResultingParser
//      }
//    }
//
//    val sampledTrees: Array[CcgTreeI] = Array(s1, s2).map(CcgTreeI.to(_, catIndexer, wordIndexer))
//
//    val mockProductionFinder: PcfgProductionCounterI = new PcfgProductionCounterI {
//      def counts(trees: Array[CcgTreeI], numTrees: Int,
//        paramKnownRoots: Array[Int], //                           ts
//        paramKnownBinys: Array[IndirectSparseVec[Array[Int]]], //         t -> u -> vs
//        paramKnownUnrys: IndirectSparseVec[Array[Int]], //                t -> us
//        paramKnownTerms: Array[Array[Int]], //                    t -> ws
//        paramNumCats: Int, paramNumWords: Int): ( // 
//        IndirectSparseVec[Int], //                                   t -> c
//        Array[IndirectSparseVec[IndirectSparseVec[Int]]], //                 t -> u -> v -> c
//        IndirectSparseVec[IndirectSparseVec[Int]], //                        t -> u -> c
//        Array[IndirectSparseVec[Int]]) //                            t -> w -> c
//        = {
//
//        assertSame(sampledTrees, trees)
//        assertEquals(sampledTrees.length, numTrees)
//        assertSame(knownRoots, paramKnownRoots)
//        assertSame(knownBinys, paramKnownBinys)
//        assertSame(knownUnrys, paramKnownUnrys)
//        assertSame(knownTerms, paramKnownTerms)
//        assertEquals(numCats, paramNumCats)
//        assertEquals(numWords, paramNumWords)
//
//        val rootCounts: IndirectSparseVec[Int] =
//          IndirectSparseVec(paramKnownRoots.map { t =>
//            t -> Map(A -> 11, B -> 25, C -> 14, D -> 15).withDefaultValue(0)(catIndexer.obj(t))
//          }, numCats)
//
//        val binyCounts: Array[IndirectSparseVec[IndirectSparseVec[Int]]] =
//          paramKnownBinys.zipWithIndex.map {
//            case (tKnownBinys, t) =>
//              if (tKnownBinys != null) {
//                IndirectSparseVec(tKnownBinys.activePairs.map {
//                  case (u, vs) =>
//                    u -> IndirectSparseVec(vs.map { v =>
//                      val m = Map(
//                        A -> Map(BinaryProd(B, C) -> 45, BinaryProd(E, F) -> 26),
//                        C -> Map(BinaryProd(D, E) -> 47, BinaryProd(E, F) -> 27),
//                        D -> Map(BinaryProd(B, C) -> 23))
//                        .mapVals(_.withDefaultValue(0))
//                        .withDefaultValue(Map[BinaryProd, Int]().withDefaultValue(0))
//                      v -> m(catIndexer.obj(t))(BinaryProd(catIndexer.obj(u), catIndexer.obj(v)))
//                    }, numCats)
//                }, numCats)
//              }
//              else null
//          }
//
//        val unryCounts: IndirectSparseVec[IndirectSparseVec[Int]] =
//          IndirectSparseVec(paramKnownUnrys.activePairs.map {
//            case (t, us) =>
//              t -> IndirectSparseVec(us.map { u =>
//                val m = Map(
//                  A -> Map(UnaryProd(B) -> 88, UnaryProd(E) -> 66),
//                  C -> Map(UnaryProd(D) -> 90, UnaryProd(E) -> 67),
//                  D -> Map(UnaryProd(B) -> 26))
//                  .mapVals(_.withDefaultValue(0))
//                  .withDefaultValue(Map[UnaryProd, Int]().withDefaultValue(0))
//                u -> m(catIndexer.obj(t))(UnaryProd(catIndexer.obj(u)))
//              }, numCats)
//          }, numCats)
//
//        val termCounts: Array[IndirectSparseVec[Int]] =
//          paramKnownTerms.zipWithIndex.map {
//            case (ws, t) =>
//              if (ws != null) {
//                IndirectSparseVec(ws.map { w =>
//                  val m = Map(
//                    A -> Map(TermProd("a1") -> 62, TermProd("a2") -> 36),
//                    C -> Map(TermProd("c1") -> 65, TermProd("c2") -> 29, TermProd("c3") -> 38),
//                    E -> Map(TermProd("e1") -> 39))
//                    .mapVals(_.withDefaultValue(0))
//                    .withDefaultValue(Map[TermProd, Int]().withDefaultValue(0))
//                  w -> m(catIndexer.obj(t))(TermProd(wordIndexer.obj(w)))
//                }, numWords)
//              }
//              else null
//          }
//
//        (rootCounts, binyCounts, unryCounts, termCounts)
//      }
//    }
//
//    val goldRootCounts: IndirectSparseVec[Int] =
//      IndirectSparseVec(knownRoots.map { t =>
//        t -> Map(A -> 21, D -> 22).withDefaultValue(0)(catIndexer.obj(t))
//      }, numCats)
//
//    val goldBinyCounts: Array[IndirectSparseVec[IndirectSparseVec[Int]]] =
//      knownBinys.zipWithIndex.map {
//        case (tKnownBinys, t) =>
//          if (tKnownBinys != null) {
//            IndirectSparseVec(tKnownBinys.activePairs.map {
//              case (u, vs) =>
//                u -> IndirectSparseVec(vs.map { v =>
//                  val m = Map(
//                    A -> Map(BinaryProd(B, C) -> 11),
//                    C -> Map(BinaryProd(D, E) -> 12, BinaryProd(E, D) -> 13))
//                    .mapVals(_.withDefaultValue(0))
//                    .withDefaultValue(Map[BinaryProd, Int]().withDefaultValue(0))
//                  v -> m(catIndexer.obj(t))(BinaryProd(catIndexer.obj(u), catIndexer.obj(v)))
//                }, numCats)
//            }, numCats)
//          }
//          else null
//      }
//
//    val goldUnryCounts: IndirectSparseVec[IndirectSparseVec[Int]] =
//      IndirectSparseVec(knownUnrys.activePairs.map {
//        case (t, us) =>
//          t -> IndirectSparseVec(us.map { u =>
//            val m = Map(
//              A -> Map(UnaryProd(B) -> 14),
//              C -> Map(UnaryProd(A) -> 15, UnaryProd(D) -> 16))
//              .mapVals(_.withDefaultValue(0))
//              .withDefaultValue(Map[UnaryProd, Int]().withDefaultValue(0))
//            u -> m(catIndexer.obj(t))(UnaryProd(catIndexer.obj(u)))
//          }, numCats)
//      }, numCats)
//
//    val goldTermCounts: Array[IndirectSparseVec[Int]] =
//      knownTerms.zipWithIndex.map {
//        case (ws, t) =>
//          if (ws != null) {
//            IndirectSparseVec(ws.map { w =>
//              val m = Map(
//                A -> Map(TermProd("a1") -> 17),
//                C -> Map(TermProd("c1") -> 18))
//                .mapVals(_.withDefaultValue(0))
//                .withDefaultValue(Map[TermProd, Int]().withDefaultValue(0))
//              w -> m(catIndexer.obj(t))(TermProd(wordIndexer.obj(w)))
//            }, numWords)
//          }
//          else null
//      }
//
//    val absct =
//      new AlphaBetaSupPcfgTrainerI(
//        priorRootDist: IndirectSparseVec[LogDouble], //                         t -> p
//        priorBinyDist: Array[IndirectSparseVec[IndirectSparseVec[LogDouble]]], //       t -> u -> v -> p
//        priorUnryDist: IndirectSparseVec[IndirectSparseVec[LogDouble]], //              t -> u -> p
//        priorTermDist: Array[IndirectSparseVec[LogDouble]], //                  t -> w -> p
//        alphaRoot: LogDouble, alphaBiny: LogDouble, alphaUnry: LogDouble, alphaTerm: LogDouble,
//        alphaLambda: LogDouble, priorBinyProdMix = LogDouble(0.5), priorUnryProdMix = LogDouble(0.4), priorTermProdMix = LogDouble(0.2),
//        mockProductionFinder: PcfgProductionCounterI,
//        mockPcfgParserInstantiater: PcfgParserInstantiaterI,
//        knownRoots: Array[Int], //                               ts
//        knownBinys: Array[IndirectSparseVec[Array[Int]]], //             t -> u -> vs
//        knownUnrys: IndirectSparseVec[Array[Int]], //                    t -> us
//        knownTerms: Array[Array[Int]], //                        t -> ws
//        numCats: Int, numWords: Int,
//        goldRootCounts: IndirectSparseVec[Int], //                         t -> c
//        goldBinyCounts: Array[IndirectSparseVec[IndirectSparseVec[Int]]], //       t -> u -> v -> c
//        goldUnryCounts: IndirectSparseVec[IndirectSparseVec[Int]], //              t -> u -> c
//        goldTermCounts: Array[IndirectSparseVec[Int]] //                   t -> w -> c
//        )(catIndexer: Indexer[Cat], wordIndexer: Indexer[String])
//
//    val parser: PcfgParserI = absct.train(sampledTrees)
//
//    assertSame(mockResultingParser, parser)
//  }
//
//  def assertEqualsLog(a: LogDouble, b: LogDouble, e: Double) {
//    assertEquals(a.toDouble, b.toDouble, e)
//  }
//}
