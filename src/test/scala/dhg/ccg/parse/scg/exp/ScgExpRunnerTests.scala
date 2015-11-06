package dhg.ccg.parse.scg.exp

import org.junit.Test
import org.junit.Assert._
import dhg.ccg.parse._
import dhg.ccg.prob._
import dhg.ccg.cat._
import dhg.ccg.rule._
import dhg.ccg.tagdict.SimpleTagDictionary
import dhg.ccg.test.TestUtil._
import dhg.ccg.parse.pcfg._
import dhg.util._
import org.apache.commons.math3.random.MersenneTwister
import dhg.ccg.parse.scg._
import dhg.ccg.parse.pcfg.mcmc.PcfgTreeSampler
import dhg.ccg.tagdict.SimpleStartEndTags
import org.apache.commons.math3.random.RandomGenerator
import dhg.ccg.tagdict.StartEndTags
import scala.collection.parallel.immutable.ParVector
import dhg.util._
import dhg.ccg.parse.dep.ParserEvaluator
import dhg.ccg.tagdict.TagDictionary

class ScgExpRunnerTests {

  val S = cat"S".asInstanceOf[AtomCat]
  val NP = cat"NP".asInstanceOf[AtomCat]
  val N = cat"N".asInstanceOf[AtomCat]
  val PP = cat"PP".asInstanceOf[AtomCat]
  val STA = cat"<S>"
  val END = cat"<E>"
  val SE = new SimpleStartEndTags(STA, END)

  val A = cat"A".asInstanceOf[AtomCat]
  val B = cat"B".asInstanceOf[AtomCat]
  val C = cat"C".asInstanceOf[AtomCat]
  val D = cat"D".asInstanceOf[AtomCat]
  val E = cat"E".asInstanceOf[AtomCat]
  val F = cat"F".asInstanceOf[AtomCat]

  @Test
  def integration_SimpleScgExpRunner_scg_mcmc_unicatgram_combine {
    type Word = String

    val modelOpt = "scg"
    val learningOpt = "mcmc"

    /* TAG DICT DATA                                        TAG DICT
     *                                                      the    NP/N
     *        S                        S                    fat    N/N
     *      /   \               +------+-----+              cats   N, NP
     *     NP    \             NP           S\NP            dogs   N
     *    /  \    \           /  \         /    \           dinner N
     * NP/N   N   S\NP     NP/N   N  (S\NP)/NP   NP         Mary   np/pp, np
     * the   / \  walk     the  dogs   chase    cats        with   pp/np
     *     N/N  N                                           walk   S\NP 
     *     fat cats                                         ate    ((s\np)/pp)/np
     *                                                      chase  (S\NP)/NP
     *                                                      
     *                                                      FULL: NP/N
     *                                                            N/N
     *                                                            N
     *                                                            NP/PP
     *                                                            NP
     *                                                            PP/NP
     *                                                            S\NP
     *                                                            ((S\NP)/PP)/NP
     *                                                            (S\NP)/NP  
     *        np
     *   +----+---- pp
     *   |      +---+--- np
     *   |      |       /  \
     * np/pp  pp/np  np/n   n
     * Mary   with   the   dogs
     *
     *                    s\np
     *              +------+----- pp 
     *         (s\np)/pp         /  \
     *         /       \        /    \
     * ((s\np)/pp)/np  np>n   pp/np   np
     *      ate       dinner  with   Mary
     * 
     * RAW DATA
     * 
     *        S                   S --------+                   
     *      /   \               /          S\NP -----+ 
     *     NP    \             NP          /         NP
     *    /  \    \            |          /         /  \
     * NP/N   N   S\NP         N    (S\NP)/NP)   NP/N   N 
     * the  dogs  walk       dogs     chase      some  cats
     * 
     *              s                                                         s
     *              |                                                         |
     *    +---------+------- s\np                                 +-----------+------------s\np
     *    |                   |                                   |                         |
     *    |         +----------------- np                         |            (s\np)/pp ---+-------- pp
     *    |         |                  |                          |                |                  |
     *    |         |            +-----+----- pp          OR      |                |            +-----+--- np
     *    |         |            |            |                   |                |            |          |
     *    |         |            |      +-----+--- np             |         +------+-----+      |      +---+---+
     *    |         |            |      |          |              |         |            |      |      |       |
     *    |         |            |      |      +---+---+         np   ((s\np)/pp)/np     np   pp/np   np/n     n
     *    |         |            |      |      |       |        John       saw          Mary   with   the   telescope
     *   np     (s\np)/np      np/pp   pp/np  np/n     n
     *	John       saw          Mary   with   the   telescope
     * 
     * 
     * DEV DATA
     * 
     *        S                   S           
     *      /   \               /   \       
     *     NP    \             NP    \     
     *    /  \    \           /  \    \
     * NP/N   N   S\NP     NP/N   N   S\NP    
     * the  dogs  walk     the  dogs  run
     * 
     */

    val mockTreeBankReader = new MockableTreeBankReader {
      override def tdData(): Iterator[CcgTree] = Iterator(
        CcgBinode(S,
          CcgBinode(NP,
            CcgLeaf(NP / N, "the", "FAKEPOS"),
            CcgBinode(N,
              CcgLeaf(N / N, "fat", "FAKEPOS"),
              CcgLeaf(N, "cats", "FAKEPOS"))),
          CcgLeaf(S \ NP, "walk", "FAKEPOS")),
        CcgBinode(S,
          CcgBinode(NP,
            CcgLeaf(NP / N, "the", "FAKEPOS"),
            CcgLeaf(N, "dogs", "FAKEPOS")),
          CcgBinode(S \ NP,
            CcgLeaf((S \ NP) / NP, "chase", "FAKEPOS"),
            CcgLeaf(NP, "cats", "FAKEPOS"))),
        CcgBinode(NP,
          CcgLeaf(NP / PP, "Mary", "FAKEPOS"),
          CcgBinode(PP,
            CcgLeaf(PP / NP, "with", "FAKEPOS"),
            CcgBinode(NP,
              CcgLeaf(NP / N, "the", "FAKEPOS"),
              CcgLeaf(N, "dogs", "FAKEPOS")))),
        CcgBinode(S \ NP,
          CcgBinode((S \ NP) / PP,
            CcgLeaf(((S \ NP) / PP) / NP, "ate", "FAKEPOS"),
            CcgLeaf(N, "dinner", "FAKEPOS")),
          CcgBinode(PP,
            CcgLeaf(PP / NP, "with", "FAKEPOS"),
            CcgLeaf(NP, "Mary", "FAKEPOS"))))
      def raw(): Iterator[Vector[Word]] = rawDataDONTUSE.map(_.leaves.map(_.word))
      override def rawDataDONTUSE: Iterator[CcgTree] = Iterator(
        CcgBinode(S,
          CcgBinode(NP,
            CcgLeaf(NP / N, "the", "FAKEPOS"),
            CcgLeaf(N, "dogs", "FAKEPOS")),
          CcgLeaf(S \ NP, "walk", "FAKEPOS")),
        CcgBinode(S,
          CcgUnode(NP,
            CcgLeaf(N, "dogs", "FAKEPOS")),
          CcgBinode(S \ NP,
            CcgLeaf((S \ NP) / NP, "chase", "FAKEPOS"),
            CcgBinode(NP,
              CcgLeaf(NP / N, "some", "FAKEPOS"),
              CcgLeaf(N, "cats", "FAKEPOS")))),
        CcgBinode(S,
          CcgLeaf(NP, "John", "FAKEPOS"),
          CcgBinode(S \ NP,
            CcgLeaf((S \ NP) / NP, "saw", "FAKEPOS"),
            CcgBinode(NP,
              CcgLeaf(NP / PP, "Mary", "FAKEPOS"),
              CcgBinode(PP,
                CcgLeaf(PP / NP, "with", "FAKEPOS"),
                CcgBinode(NP,
                  CcgLeaf(NP / N, "the", "FAKEPOS"),
                  CcgLeaf(N, "telescope", "FAKEPOS")))))))
      override def devData(): Iterator[CcgTree] = Iterator(
        CcgBinode(S, CcgBinode(NP, CcgLeaf(NP / N, "the", "FAKEPOS"), CcgLeaf(N, "dogs", "FAKEPOS")), CcgLeaf(S \ NP, "walk", "FAKEPOS")),
        CcgBinode(S, CcgBinode(NP, CcgLeaf(NP / N, "the", "FAKEPOS"), CcgLeaf(N, "dogs", "FAKEPOS")), CcgLeaf(S \ NP, "run", "FAKEPOS")))

      override def testData(): Iterator[CcgTree] = Iterator()
      def readFileTrees(file: File): Iterator[CcgTree] = ???
      def readFileTrees(fileNumber: Int): Option[Iterator[CcgTree]] = ???
    }

    val mockParserEvaluator = new ParserEvaluator {
      def evaluate(model: GuideChartParser, testData: TraversableOnce[(Option[CfgGuideChart], CcgTree)], tagdict: TagDictionary[Cat],
        printFailedParses: Boolean = false,
        verbose: Boolean = false): Double = {
        println(f"\nMOCK EVALUATOR")
        for ((gco, tree) <- testData) {
          gco match {
            case Some(gc) =>
              val Some((t, p)) = model.parseAndProbFromGuideChart(gc)
              println(f"        $p  $t")
          }
        }
        println(f"")
        -1
      }
    }

    val runner =
      new SimpleScgExpRunner(
        rules = Vector(FA, BA, N2NP),
        mockTreeBankReader,
        tdTokToUse = Int.MaxValue,
        numTrainSentences = Int.MaxValue,
        numTestSentences = Int.MaxValue,

        samplingIterations = 5,
        burninIterations = 0,
        //      maxEmIterations: Int,
        //      maxDecompIterations: Int,

        alphaRoot = 1.1,
        alphaBiny = 1.2,
        alphaUnry = 1.3,
        alphaTerm = 1.4,
        alphaProd = 1.5,
        alphaLctx = 1.6,
        alphaRctx = 1.7,
        alphaTran = 1.8,
        alphaEmis = 1.9,
        alphaLambda = 3.0,
        priorBinyProdMix = 0.2,
        priorUnryProdMix = 0.3,
        priorTermProdMix = 0.5,

        modelOpt: String,
        learningOpt: String,

        rootinit = "catprior",
        ntprodInit = "catprior",
        termprodInit = "uniform",
        trinit = "combine-tdentry",

        pTerm = 0.6,
        pMod = 0.2,
        pFwd = 0.55,

        //catpriorMass = 0.65,
        combinableTransitionMass = 0.85,

        tdIncludesRequiredMappings = false,
        tdCutoff = 0.0,
        tdIncludesTest = false,

        maxAcceptanceTries = 1,
        parseCountCutoff = -1L,
        //numSamples :Int,

        mockParserEvaluator,

        punctSplit = false,
        trainAnnotator = "none",
        trainAnnotatedSentencesProportion = 0.0,
        trainBracketProportion = 0.0,
        trainBracketHighMatchBase = false,
        trainBracketCats = (_ => true),
        trainBaseCats = true,
        trainHighCats = true,
        trainDepProportion = 0.0,
        trainGCB = "x",
        testAnnotator = "none",
        testAnnotatedSentencesProportion = 0.0,
        testBracketHighMatchBase = false,
        testBracketProportion = 0.0,
        testBracketCats = (_ => true),
        testBaseCats = true,
        testHighCats = true,
        testDepProportion = 0.0,
        testGCB = "fb",

        numClusters = None,

        useSerializedTrainGC = false,
        useSerializedTestGC = false,
        trainSerializedGC = None,
        testSerializedGC = None,

        sentencesToTake = Int.MaxValue,
        maxTrainTokens = Int.MaxValue,

        argString = "arg-string",

        verbose = true)

    /* Category Prior
     * 
     *   Atoms: S, NP, N, PP
     *   
     *   N               ->  0.6*(1/6.0)                              = 0.1             / Z = 
     *   NP              ->  0.6*(1/6.0)                              = 0.1             / Z = 
     *   NP/N            ->  (1-0.6)*(1-0.2)*0.55 * 0.1 * 0.1         = 0.00176         / Z = 
     *   N/N             ->  (1-0.6)*   0.2 *0.55 * 0.1               = 0.0044          / Z = 
     *   NP/PP           ->  (1-0.6)*(1-0.2)*0.55 * 0.1 * 0.1         = 0.00176         / Z = 
     *   PP/NP           ->  (1-0.6)*(1-0.2)*0.55 * 0.1 * 0.1         = 0.00176         / Z = 
     *   S\NP            ->  (1-0.6)*(1-0.2)*0.45 * 0.1 * 0.1         = 0.00144         / Z = 
     *   (S\NP)/NP       ->  (1-0.6)*(1-0.2)*0.55 * 0.00144     * 0.1 = 0.000025344     / Z = 
     *   ((S\NP)/PP)/NP  ->  (1-0.6)*(1-0.2)*0.55 * 0.000025344 * 0.1 = 0.0000004460544 / Z = 
     *   <S>             ->  0.6*(1/6.0)                              = 0.1             / Z = 
     *   <E>             ->  0.6*(1/6.0)                              = 0.1             / Z = 
     *                                                                  
     */

    /* 
     * AlphaPriorCounts = alpha * priorDist/Z + goldCounts
     * 
     * alphaPriorRootCounts:
     *   S  -> 1.1 * 0.1 = 0.11 
     *   NP -> 1.1 * 0.1 = 0.11
     *   
     * alphaPriorBinyCounts:
     *   S ->           [NP (S\NP)]            = 1.2 * 0.1 * 0.00144 					= 1.728E-4
     *  
     *   NP ->          [(NP/N) N]             = 1.2 * 0.00176 * 0.1 					= 2.112E-4
     *   NP ->          [(NP/PP) PP]           = 1.2 * 0.00176 * 0.1 					= 2.112E-4
     *   
     *   N ->           [(N/N) N]              = 1.2 * 0.0044 * 0.1 					= 5.28E-4
     *   
     *   (S\NP) ->      [((S\NP)/NP) NP]       = 1.2 * 0.000025344 * 0.1 			= 3.04128E-6
     *   (S\NP) ->      [((S\NP)/PP) PP]       = 1.2 * 0.000025344 * 0.1 			= 3.04128E-6
     *   
     *   ((S\NP)/PP) -> [(((S\NP)/PP)/NP) NP]  = 1.2 * 0.0000004460544 * 0.1 	= 5.3526528E-8
     *   
     *   PP ->          [(PP/NP) NP]           = 1.2 * 0.00176 * 0.1 					= 2.112E-4
     *   
     * alphaPriorUnryCounts:
     *   NP -> N  = 1.3 * 0.1 = 0.13
     *   
     * alphaPriorTermCounts:
     *   N -> "cats" 							= 1.4 * 1/15 = 0.09333333333333332
     *   (S\NP)/NP -> "chase"			= 
     *   N -> "dogs"							= 
     *   N -> "John"							= 
     *   NP -> "John"  						= 
     *   NP/PP -> "John"					= 
     *   NP -> "Mary"							= 
     *   NP/PP -> "Mary"					= 
     *   (S\NP)/NP -> "saw"				=
     *   ((S\NP)/PP)/NP -> "saw"	= 
     *   PP/NP -> "saw"						= 
     *   NP/N -> "some"						= 
     *   N/N -> "some"						= 
     *   N -> "telescope"					= 
     *   NP/N -> "the"						= 
     *   S\NP -> "walk"						= 
     *   PP/NP -> "with"					= 
     *   
     *   
     * the    dogs   walk
     * NP/N    N     S\NP  
     *        
     * dogs     chase      some     cats
     *  N     (S\NP)/NP    {  }      N
     * 
     * John     saw     Mary      with     the    telescope       x2
     * {  }     { }     NP/PP     PP/NP    NP/N      { }
     *  				        NP    						   
     * 
     *                     2.     3.  4.    5.         6.     7.  8.     9.   11.
     *                     NP/N   N   S\NP  (S\NP)/NP  NP/PP  NP  PP/NP  N/N  <E>
     *   1.  <S>             1    1                                                    / 2
     *   2.  NP/N                 2                                                    / 2
     *   3.  N                         1        1                              1       / 2
     *   4.  S\NP                                                              1       / 1
     *   5.  (S\NP)/NP                                                                 / 0
     *   6.  NP/PP                                                 0.5                 / 0.5
     *   7.  NP                                                    0.5                 / 0.5
     *   8.  PP/NP           1                                                         / 1
     *   9.  N/N                                                                       / 0
     *  10.  ((S\NP)/PP)/NP                                                            / 0
     *                      ----  ---  ---   -------   ----  ---  -----  ---  ---
     *                        2    3    1       1        0    0     1     0    2
     * 
     * 
     * alphaPriorLctxCounts
     * 2.
     *   <s> <- np/n              = (1 + 0.1) / (2 + 10*0.1) = 0.3666666666666667                          / 0.8 * 0.85 = 0.38958333333333334  * 1.6 = 0.6233333333333334      **
     *   np/n <- np/n             = (0 + 0.1) / (2 + 10*0.1) =                        0.03333333333333333  / 0.2 * 0.15 = 0.025                * 1.6 = 0.04
     *   n <- np/n                = (0 + 0.1) / (2 + 10*0.1) =                        0.03333333333333333  / 0.2 * 0.15 = 0.025                * 1.6 = 0.04
     *   s\np <- np/n             = (0 + 0.1) / (2 + 10*0.1) =                        0.03333333333333333  / 0.2 * 0.15 = 0.025                * 1.6 = 0.04
     *   (s\np)/np <- np/n        = (0 + 0.1) / (2 + 10*0.1) = 0.03333333333333333                         / 0.8 * 0.85 = 0.035416666666666666 * 1.6 = 0.05666666666666667     **
     *   np/pp <- np/n            = (0 + 0.1) / (2 + 10*0.1) =                        0.03333333333333333  / 0.2 * 0.15 = 0.025                * 1.6 = 0.04
     *   np <- np/n               = (0 + 0.1) / (2 + 10*0.1) =                        0.03333333333333333  / 0.2 * 0.15 = 0.025                * 1.6 = 0.04
     *   pp/np <- np/n            = (1 + 0.1) / (2 + 10*0.1) = 0.3666666666666667                          / 0.8 * 0.85 = 0.38958333333333334  * 1.6 = 0.6233333333333334      **
     *   n/n <- np/n              = (0 + 0.1) / (2 + 10*0.1) =                        0.03333333333333333  / 0.2 * 0.15 = 0.025                * 1.6 = 0.04
     *   ((s\np)/pp)/np <- np/n   = (0 + 0.1) / (2 + 10*0.1) = 0.03333333333333333                         / 0.8 * 0.85 = 0.035416666666666666 * 1.6 = 0.05666666666666667
     *                                                         -------------------    -------------------
     *                                                         0.8                    0.2
     * 
     * 8.
     *   <s> <- pp/np              = (0.0 + 0.1) / (1 + 10*0.1) = 0.05                         / 0.35  * 0.85 = 0.12142857142857144   * 1.6 = 0.1942857142857143     
     *   np/n <- pp/np             = (0.0 + 0.1) / (1 + 10*0.1) =                        0.05  / 0.65  * 0.15 = 0.011538461538461539  * 1.6 = 0.018461538461538463
     *   n <- pp/np                = (0.0 + 0.1) / (1 + 10*0.1) =                        0.05  / 0.65  * 0.15 = 0.011538461538461539  * 1.6 = 0.018461538461538463   **
     *   s\np <- pp/np             = (0.0 + 0.1) / (1 + 10*0.1) =                        0.05  / 0.65  * 0.15 = 0.011538461538461539  * 1.6 = 0.018461538461538463
     *   (s\np)/np <- pp/np        = (0.0 + 0.1) / (1 + 10*0.1) =                        0.05  / 0.65  * 0.15 = 0.011538461538461539  * 1.6 = 0.018461538461538463
     *   np/pp <- pp/np            = (0.5 + 0.1) / (1 + 10*0.1) = 0.3                          / 0.35  * 0.85 = 0.7285714285714286    * 1.6 = 1.165714285714286      **
     *   np <- pp/np               = (0.5 + 0.1) / (1 + 10*0.1) =                        0.3   / 0.65  * 0.15 = 0.06923076923076922   * 1.6 = 0.11076923076923076    **
     *   pp/np <- pp/np            = (0.0 + 0.1) / (1 + 10*0.1) =                        0.05  / 0.65  * 0.15 = 0.011538461538461539  * 1.6 = 0.018461538461538463
     *   n/n <- pp/np              = (0.0 + 0.1) / (1 + 10*0.1) =                        0.05  / 0.65  * 0.15 = 0.011538461538461539  * 1.6 = 0.018461538461538463
     *   ((s\np)/pp)/np <- pp/np   = (0.0 + 0.1) / (1 + 10*0.1) =                        0.05  / 0.65  * 0.15 = 0.011538461538461539  * 1.6 = 0.018461538461538463
     *                                                            -------------------    ----
     *                                                            0.35                   0.65
     *
     * 11.
     *   <s> <- <e>              = 0.0
     *   np/n <- <e>             = (0 + 0.1) / (2 + 9*0.1) =                        0.034482758620689655  / 0.20689655172413793  * 0.15 = 0.025                * 1.6 = 0.04
     *   n <- <e>                = (1 + 0.1) / (2 + 9*0.1) = 0.37931034482758624                          / 0.7931034482758621   * 0.85 = 0.40652173913043477  * 1.6 = 0.6504347826086957
     *   n/n <- <e>              = (0 + 0.1) / (2 + 9*0.1) =                        0.034482758620689655  / 0.20689655172413793  * 0.15 = 0.025                * 1.6 = 0.04
     *   np/pp <- <e>            = (0 + 0.1) / (2 + 9*0.1) =                        0.034482758620689655  / 0.20689655172413793  * 0.15 = 0.025                * 1.6 = 0.04
     *   np <- <e>               = (0 + 0.1) / (2 + 9*0.1) = 0.034482758620689655                         / 0.7931034482758621   * 0.85 = 0.03695652173913043  * 1.6 = 0.05913043478260869
     *   pp/np <- <e>            = (0 + 0.1) / (2 + 9*0.1) =                        0.034482758620689655  / 0.20689655172413793  * 0.15 = 0.025                * 1.6 = 0.04
     *   s\np <- <e>             = (1 + 0.1) / (2 + 9*0.1) = 0.37931034482758624                          / 0.7931034482758621   * 0.85 = 0.40652173913043477  * 1.6 = 0.6504347826086957
     *   ((s\np)/pp)/np <- <e>   = (0 + 0.1) / (2 + 9*0.1) =                        0.034482758620689655  / 0.20689655172413793  * 0.15 = 0.025                * 1.6 = 0.04
     *   (s\np)/np <- <e>        = (0 + 0.1) / (2 + 9*0.1) =                        0.034482758620689655  / 0.20689655172413793  * 0.15 = 0.025                * 1.6 = 0.04
     *                                                       -------------------    -------------------
     *                                                       0.7931034482758621     0.20689655172413793
     *   
     *  
     *  
     * alphaPriorRctxCounts
     * 7.
     *   np -> np/n             = (0.0 + 0.1) / (0.5 + 10*0.1) =                        0.06666666666666667  / 0.7333333333333333  * 0.15 = 0.013636363636363636  * 1.7 = 0.02318181818181818
     *   np -> n                = (0.0 + 0.1) / (0.5 + 10*0.1) =                        0.06666666666666667  / 0.7333333333333333  * 0.15 = 0.013636363636363636  * 1.7 = 0.02318181818181818
     *   np -> s\np             = (0.0 + 0.1) / (0.5 + 10*0.1) = 0.06666666666666667                         / 0.26666666666666666 * 0.85 = 0.2125                * 1.7 = 0.36125               **
     *   np -> (s\np)/np        = (0.0 + 0.1) / (0.5 + 10*0.1) = 0.06666666666666667                         / 0.26666666666666666 * 0.85 = 0.2125                * 1.7 = 0.36125               **
     *   np -> np/pp            = (0.0 + 0.1) / (0.5 + 10*0.1) =                        0.06666666666666667  / 0.7333333333333333  * 0.15 = 0.013636363636363636  * 1.7 = 0.02318181818181818
     *   np -> np               = (0.0 + 0.1) / (0.5 + 10*0.1) =                        0.06666666666666667  / 0.7333333333333333  * 0.15 = 0.013636363636363636  * 1.7 = 0.02318181818181818
     *   np -> pp/np            = (0.5 + 0.1) / (0.5 + 10*0.1) =                        0.4                  / 0.7333333333333333  * 0.15 = 0.08181818181818183   * 1.7 = 0.13909090909090909   **
     *   np -> n/n              = (0.0 + 0.1) / (0.5 + 10*0.1) =                        0.06666666666666667  / 0.7333333333333333  * 0.15 = 0.013636363636363636  * 1.7 = 0.02318181818181818
     *   np -> ((s\np)/pp)/np   = (0.0 + 0.1) / (0.5 + 10*0.1) = 0.06666666666666667                         / 0.26666666666666666 * 0.85 = 0.2125                * 1.7 = 0.36125               **
     *   np -> <e>              = (0.0 + 0.1) / (0.5 + 10*0.1) = 0.06666666666666667                         / 0.26666666666666666 * 0.85 = 0.2125                * 1.7 = 0.36125               **
     *                                                           -------------------    -------------------
     *                                                           0.26666666666666666    0.7333333333333333
     *   
     */

    val result = runner.run()

    throw new NotImplementedError("Asserts not written")
  }

}
