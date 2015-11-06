package dhg.ccg.prob

import dhg.util._
import org.junit.Test
import dhg.util.TestUtil._
import org.junit.Assert._

class LogProbabilityDistributionTests {

  @Test
  def emptyPD {
    val pd = LogProbabilityDistribution.empty[Symbol]
    assertEqualsLog(LogDouble(0.0), pd('z), 1e-9)
    assertException(pd.sample()) { case e: AssertionError => assertEquals("assertion failed: cannot sample from an empty distribution", e.getMessage) }
    assertEqualsLog(LogDouble(0.0), pd.defaultProb, 1e-9)
  }

  @Test
  def emptyAbstractPD_withDefault {
    val pd = new AbstractLogProbabilityDistribution[Symbol](LogDouble(2.0)) {
      override protected[this] def allKnownBs = Set.empty
      override protected[this] def counts(b: Symbol) = LogDouble(1.0)
      override protected[this] def defaultCount: LogDouble = LogDouble(0.5)
    }
    assertEqualsLog(LogDouble(0.5), pd('z), 1e-9)
    assertException(pd.sample()) { case e: AssertionError => assertEquals("assertion failed: cannot sample from an empty distribution", e.getMessage) }
    assertEqualsLog(LogDouble(0.25), pd.defaultProb, 1e-9)
  }

  @Test
  def masslessAbstractPD {
    val pd = new AbstractLogProbabilityDistribution[Symbol](LogDouble(2.0)) {
      override protected[this] def allKnownBs = Set('a, 'b, 'c)
      override protected[this] def counts(b: Symbol) = LogDouble.zero
      override protected[this] def defaultCount: LogDouble = LogDouble(1.0)
    }
    assertEqualsLog(LogDouble(0.0), pd('a), 1e-9)
    assertEqualsLog(LogDouble(0.0), pd('b), 1e-9)
    assertEqualsLog(LogDouble(0.0), pd('c), 1e-9)
    assertEqualsLog(LogDouble(0.0), pd('z), 1e-9)

    assertException(pd.sample()) { case e: AssertionError => assertEquals("assertion failed: cannot sample from a distribution without mass", e.getMessage) }

    assertEqualsLog(LogDouble(0.5), pd.defaultProb, 1e-9)
  }

  @Test
  def bugfix_identicalCounts {
    val pd = new AbstractLogProbabilityDistribution[Symbol](LogDouble(1.0)) {
      override protected[this] def allKnownBs = Set('a, 'b)
      override protected[this] def counts(b: Symbol) = LogDouble(2.0)
      override protected[this] def defaultCount: LogDouble = LogDouble(0.5)
    }
    assertEqualsLog(LogDouble(0.4), pd('a), 1e-9)
    assertEqualsLog(LogDouble(0.4), pd('b), 1e-9)
    assertEqualsLog(LogDouble(0.4), pd('z), 1e-9)

    val samples = Vector.fill(1000000)(pd.sample())
    val proportions = samples.counts.normalizeValues
    assertEquals(2, proportions.size)
    assertEquals(0.5, proportions('a), 0.01)
    assertEquals(0.5, proportions('b), 0.01)

    assertEqualsLog(LogDouble(0.1), pd.defaultProb, 1e-9)
  }

  @Test
  def simplePD {
    val pd = new SimpleLogProbabilityDistribution(Map('a -> LogDouble(1.0), 'b -> LogDouble(2.0), 'c -> LogDouble(7.0)))
    assertEqualsLog(LogDouble(0.1), pd('a), 1e-9)
    assertEqualsLog(LogDouble(0.2), pd('b), 1e-9)
    assertEqualsLog(LogDouble(0.7), pd('c), 1e-9)
    assertEqualsLog(LogDouble(0.0), pd('z), 1e-9)

    val samples = Vector.fill(1000000)(pd.sample())
    val proportions = samples.counts.normalizeValues
    assertEquals(3, proportions.size)
    assertEquals(0.1, proportions('a), 0.01)
    assertEquals(0.2, proportions('b), 0.01)
    assertEquals(0.7, proportions('c), 0.01)

    assertEqualsLog(LogDouble(0.0), pd.defaultProb, 1e-9)
  }

  @Test
  def laplacePD {
    val pd = new LaplaceLogProbabilityDistribution(Map('a -> LogDouble(1.0), 'b -> LogDouble(2.0), 'c -> LogDouble(4.0)), None, None, LogDouble(1.0), LogDouble(0.0))
    assertEqualsLog(LogDouble(0.2), pd('a), 1e-9) // 1 + 1 = 2    / 10 = 0.2
    assertEqualsLog(LogDouble(0.3), pd('b), 1e-9) // 2 + 1 = 3    / 10 = 0.3
    assertEqualsLog(LogDouble(0.5), pd('c), 1e-9) // 4 + 1 = 5    / 10 = 0.5
    //                                                      --
    //                                                      10
    assertEqualsLog(LogDouble(0.1), pd('z), 1e-9) // 0 + 1 = 1    / 10 = 0.1

    val samples = Vector.fill(1000000)(pd.sample())
    val proportions = samples.counts.normalizeValues
    assertEquals(3, proportions.size)
    assertEquals(0.2, proportions('a), 0.01)
    assertEquals(0.3, proportions('b), 0.01)
    assertEquals(0.5, proportions('c), 0.01)

    assertEqualsLog(LogDouble(0.1), pd.defaultProb, 1e-9)
  }

  @Test
  def laplacePD_knownExclude {
    val pd = new LaplaceLogProbabilityDistribution(Map('a -> LogDouble(1.0), 'b -> LogDouble(2.0), 'c -> LogDouble(3.0), 'e -> LogDouble(8.0)), Some(Set('d)), Some(Set('e)), LogDouble(1.0), LogDouble(0.0))
    assertEqualsLog(LogDouble(0.2), pd('a), 1e-9) // 1 + 1 = 2    / 10 = 0.2
    assertEqualsLog(LogDouble(0.3), pd('b), 1e-9) // 2 + 1 = 3    / 10 = 0.3
    assertEqualsLog(LogDouble(0.4), pd('c), 1e-9) // 3 + 1 = 4    / 10 = 0.4
    assertEqualsLog(LogDouble(0.1), pd('d), 1e-9) // 0 + 1 = 1    / 10 = 0.1
    //                                                      --
    //                                                      10
    assertEqualsLog(LogDouble(0.1), pd('z), 1e-9) // 0 + 1 = 1    / 10 = 0.1
    assertEqualsLog(LogDouble(0.0), pd('e), 1e-9) // excluded

    val samples = Vector.fill(1000000)(pd.sample())
    val proportions = samples.counts.normalizeValues
    assertEquals(4, proportions.size)
    assertEquals(0.2, proportions('a), 0.01)
    assertEquals(0.3, proportions('b), 0.01)
    assertEquals(0.4, proportions('c), 0.01)
    assertEquals(0.1, proportions('d), 0.01)

    assertEqualsLog(LogDouble(0.1), pd.defaultProb, 1e-9)
  }

  @Test
  def laplacePD_totalAddition {
    val pd = new LaplaceLogProbabilityDistribution(Map('a -> LogDouble(1.0), 'b -> LogDouble(2.0), 'c -> LogDouble(4.0)), None, None, LogDouble(1.0), LogDouble(10.0))
    assertEqualsLog(LogDouble(0.10), pd('a), 1e-9) // 1 + 1 = 2    / 20 = 0.10
    assertEqualsLog(LogDouble(0.15), pd('b), 1e-9) // 2 + 1 = 3    / 20 = 0.15
    assertEqualsLog(LogDouble(0.25), pd('c), 1e-9) // 4 + 1 = 5    / 20 = 0.25
    //                                                       --
    //                                                       10+10
    assertEqualsLog(LogDouble(0.05), pd('z), 1e-9) // 0 + 1 = 1    / 20 = 0.05

    val samples = Vector.fill(1000000)(pd.sample())
    val proportions = samples.counts.normalizeValues
    assertEquals(3, proportions.size)
    assertEquals(0.20, proportions('a), 0.01)
    assertEquals(0.30, proportions('b), 0.01)
    assertEquals(0.50, proportions('c), 0.01)

    assertEqualsLog(LogDouble(0.05), pd.defaultProb, 1e-9)
  }

  @Test
  def alphaBetaPD {
    val pd = new AlphaBetaLogProbabilityDistribution(
      unsmoothedCounts = Map(
        'a -> LogDouble(2.1),
        'b -> LogDouble(3.2)),
      alpha = LogDouble(4.1),
      prior = new LogProbabilityDistribution[Symbol] {
        def apply(x: Symbol) = x match {
          case 'a => LogDouble(0.15)
          case 'b => LogDouble(0.10)
          case 'c => LogDouble(0.25)
        }
        def sample(): Symbol = ???
        def defaultProb: LogDouble = ???
      })

    assertEqualsLog(LogDouble((4.1 * 0.15 + 2.1) / (4.1 * 1.0 + 5.3)), pd('a), 1e-9)
    assertEqualsLog(LogDouble((4.1 * 0.10 + 3.2) / (4.1 * 1.0 + 5.3)), pd('b), 1e-9)
    assertEqualsLog(LogDouble((4.1 * 0.25 + 0.0) / (4.1 * 1.0 + 5.3)), pd('c), 1e-9)

    val samples = Vector.fill(1000000)(pd.sample())
    val proportions = samples.counts.normalizeValues
    assertEquals(2, proportions.size)
    assertEquals((pd('a) / Vector('a, 'b).sumBy(pd(_))).toDouble, proportions('a), 0.01)
    assertEquals((pd('b) / Vector('a, 'b).sumBy(pd(_))).toDouble, proportions('b), 0.01)

    assertExceptionMsg("`defaultProb` not available for AlphaBetaLogProbabilityDistribution")(pd.defaultProb)
  }

  @Test
  def alphaBetaPD_knownExclude {
    val pd = new AlphaBetaLogProbabilityDistribution(
      unsmoothedCounts = Map(
        'a -> LogDouble(2.1),
        'b -> LogDouble(3.2),
        'e -> LogDouble(4.3)),
      alpha = LogDouble(4.1),
      prior = new LogProbabilityDistribution[Symbol] {
        def apply(x: Symbol) = x match {
          case 'a => LogDouble(0.15)
          case 'b => LogDouble(0.10)
          case 'c => LogDouble(0.25)
          case 'd => LogDouble(0.40)
        }
        def sample(): Symbol = ???
        def defaultProb: LogDouble = ???
      },
      knownBs = Some(Set('b, 'c, 'd, 'e)),
      excludedBs = Some(Set('e)),
      totalAddition = LogDouble(10.0))

    assertEqualsLog(LogDouble((4.1 * 0.15 + 2.1) / (4.1 * 1.0 + 5.3 + 10.0)), pd('a), 1e-9)
    assertEqualsLog(LogDouble((4.1 * 0.10 + 3.2) / (4.1 * 1.0 + 5.3 + 10.0)), pd('b), 1e-9)
    assertEqualsLog(LogDouble((4.1 * 0.25 + 0.0) / (4.1 * 1.0 + 5.3 + 10.0)), pd('c), 1e-9)
    assertEqualsLog(LogDouble((4.1 * 0.40 + 0.0) / (4.1 * 1.0 + 5.3 + 10.0)), pd('d), 1e-9)
    assertEqualsLog(LogDouble.zero /*                                    */ , pd('e), 1e-9)

    val samples = Vector.fill(1000000)(pd.sample())
    val proportions = samples.counts.normalizeValues
    assertEquals(4, proportions.size)
    assertEquals((pd('a) / Vector('a, 'b, 'c, 'd).sumBy(pd(_))).toDouble, proportions('a), 0.01)
    assertEquals((pd('b) / Vector('a, 'b, 'c, 'd).sumBy(pd(_))).toDouble, proportions('b), 0.01)
    assertEquals((pd('c) / Vector('a, 'b, 'c, 'd).sumBy(pd(_))).toDouble, proportions('c), 0.01)
    assertEquals((pd('d) / Vector('a, 'b, 'c, 'd).sumBy(pd(_))).toDouble, proportions('d), 0.01)

    assertExceptionMsg("`defaultProb` not available for AlphaBetaLogProbabilityDistribution")(pd.defaultProb)
  }

  @Test
  def test_DefaultedLogProbabilityDistribution_knownExclude {
    val pd = new DefaultedLogProbabilityDistribution(Map('a -> LogDouble(1.5), 'b -> LogDouble(2.5), 'c -> LogDouble(5.5), 'e -> LogDouble(8.0)),
      Some(Set('c, 'd)), Some(Set('e)), LogDouble(0.5), LogDouble(10.0))
    assertEqualsLog(LogDouble(0.075), pd('a), 1e-9)
    assertEqualsLog(LogDouble(0.125), pd('b), 1e-9)
    assertEqualsLog(LogDouble(0.275), pd('c), 1e-9)
    assertEqualsLog(LogDouble(0.025), pd('d), 1e-9)
    assertEqualsLog(LogDouble(0.025), pd('z), 1e-9)

    val samples = Vector.fill(1000000)(pd.sample())
    val proportions = samples.counts.normalizeValues
    assertEquals(4, proportions.size)
    assertEquals(0.15, proportions('a), 0.01)
    assertEquals(0.25, proportions('b), 0.01)
    assertEquals(0.55, proportions('c), 0.01)
    assertEquals(0.05, proportions('d), 0.01)

    assertEqualsLog(LogDouble(0.025), pd.defaultProb, 1e-9)
  }

  @Test
  def bugfix_defaultOnly {
    val lpd = new LaplaceLogProbabilityDistribution[Symbol](Map(), None, None, LogDouble(1.0), LogDouble(10.0))
    val dpd = new DefaultedLogProbabilityDistribution[Symbol](Map(), None, None, LogDouble(1.0), LogDouble(10.0))
    assertEqualsLog(LogDouble(0.1), lpd('z), 1e-9)
    assertEqualsLog(LogDouble(0.1), dpd('z), 1e-9)
    assertException(lpd.sample()) { case e: AssertionError => assertEquals("assertion failed: cannot sample from an empty distribution", e.getMessage) }
    assertException(dpd.sample()) { case e: AssertionError => assertEquals("assertion failed: cannot sample from an empty distribution", e.getMessage) }
    assertEqualsLog(LogDouble(0.1), lpd.defaultProb, 1e-9)
    assertEqualsLog(LogDouble(0.1), dpd.defaultProb, 1e-9)
  }

  @Test
  def test_UniformDefaultLogProbabilityDistribution {
    val udpd = new UniformDefaultLogProbabilityDistribution[Symbol](LogDouble(0.35))

    assertEqualsLog(LogDouble(0.35), udpd('A), 1e-9)
    assertEqualsLog(LogDouble(0.35), udpd('B), 1e-9)
    assertEqualsLog(LogDouble(0.35), udpd('C), 1e-9)

    assertException(udpd.sample) { case e: RuntimeException => assertEquals("Cannot sample from a UniformDefaultLogProbabilityDistribution", e.getMessage) }

    assertEqualsLog(LogDouble(0.35), udpd.defaultProb, 1e-9)
  }

  @Test
  def test_ConditionalWrappingLogProbabilityDistribution {
    val _cpd = new ConditionalLogProbabilityDistribution[Symbol, Int] {
      def apply(x: Int, given: Symbol): LogDouble = (given, x) match {
        case ('a, 1) => LogDouble(0.5)
        case ('b, 2) => LogDouble(0.1)
        case ('c, 3) => LogDouble(0.7)
      }
      def sample(given: Symbol): Int = given match {
        case 'a => 11
        case 'b => 13
        case 'c => 17
      }
    }

    val a = new ConditionalWrappingLogProbabilityDistribution('a, _cpd)
    val b = new ConditionalWrappingLogProbabilityDistribution('b, _cpd)
    val c = new ConditionalWrappingLogProbabilityDistribution('c, _cpd)

    assertEqualsLog(LogDouble(0.5), a(1), 1e-9)
    assertEqualsLog(LogDouble(0.1), b(2), 1e-9)
    assertEqualsLog(LogDouble(0.7), c(3), 1e-9)

    assertEquals(11, a.sample())
    assertEquals(13, b.sample())
    assertEquals(17, c.sample())

    assertException(a.defaultProb) { case e: RuntimeException => assertEquals("defaultProb not available for ConditionalWrappingLogProbabilityDistribution", e.getMessage) }
  }

  @Test
  def test_UnconditionalWrappingConditionalLogProbabilityDistribution {
    val _pd = new LogProbabilityDistribution[Int] {
      def apply(x: Int) = x match {
        case 1 => LogDouble(0.5)
        case 2 => LogDouble(0.3)
      }
      def sample() = 11
      def defaultProb = LogDouble(0.1)
    }
    val cpd = new UnconditionalWrappingConditionalLogProbabilityDistribution[Symbol, Int](_pd)

    assertEqualsLog(LogDouble(0.5), cpd(1, 'a), 1e-9)
    assertEqualsLog(LogDouble(0.3), cpd(2, 'b), 1e-9)

    assertEquals(11, cpd.sample('a))
    assertEquals(11, cpd.sample('b))
  }

  @Test
  def test_SimpleConditionalLogProbabilityDistribution {
    val cpd = new SimpleConditionalLogProbabilityDistribution(Map(
      'a -> new LogProbabilityDistribution[Int] { def apply(x: Int) = x match { case 1 => LogDouble(0.5) }; def sample() = 11; def defaultProb = ??? },
      'b -> new LogProbabilityDistribution[Int] { def apply(x: Int) = x match { case 2 => LogDouble(0.3) }; def sample() = 13; def defaultProb = ??? },
      'e -> new LogProbabilityDistribution[Int] { def apply(x: Int) = ???; def sample() = ???; def defaultProb = ??? }),
      new ConditionalLogProbabilityDistribution[Symbol, Int] {
        def apply(x: Int, given: Symbol): LogDouble = (given, x) match {
          case ('b, 2) => ???
          case ('c, 3) => LogDouble(0.7)
          case ('d, 4) => LogDouble(0.1)
          case ('e, 5) => ???
        }
        def sample(given: Symbol): Int = given match {
          case 'b => ???
          case 'c => 17
          case 'd => 19
          case 'e => ???
        }
      },
      Some(Set('e)))

    assertEqualsLog(LogDouble(0.5), cpd(1, 'a), 1e-9)
    assertEqualsLog(LogDouble(0.3), cpd(2, 'b), 1e-9)
    assertEqualsLog(LogDouble(0.7), cpd(3, 'c), 1e-9)
    assertEqualsLog(LogDouble(0.1), cpd(4, 'd), 1e-9)
    assertEqualsLog(LogDouble(0.0), cpd(5, 'e), 1e-9)

    assertEquals(11, cpd.sample('a))
    assertEquals(13, cpd.sample('b))
    assertEquals(17, cpd.sample('c))
    assertEquals(19, cpd.sample('d))
    assertException(cpd.sample('e)) { case e: RuntimeException => assertEquals("cannot sample from 'e", e.getMessage) }
  }

  @Test
  def test_NormalizingConditionalLogProbabilityDistribution {
    type A = Symbol
    type B = String

    val ncpd =
      new NormalizingConditionalLogProbabilityDistribution[A, B](
        delegate = new ConditionalLogProbabilityDistribution[A, B] {
          def apply(x: B, given: A): LogDouble = LogDouble((x, given) match {
            case ("a1", 'A) => 1.1
            case ("a2", 'A) => 2.1
            case ("b1", 'B) => 1.2
            case ("b2", 'B) => 2.2
            case ("c1", 'C) => 1.3
            case ("c2", 'C) => 2.3
          })
          def sample(given: A): B = given match {
            case 'S => "s"
            case 'R => "r"
            case 'T => "t"
          }
        },
        normalizingConstants = Map[A, LogDouble](
          'A -> LogDouble(4.1),
          'C -> LogDouble(4.3))) {

      }

    //    def apply(x: B, given: A): LogDouble = delegate(x, given) / normalizingConstants.getOrElse(given, LogDouble.one)
    //		def sample(given: A): B = delegate.sample(given)

    assertEqualsLog(LogDouble(1.1 / 4.1), ncpd("a1", 'A), 1e-9)
    assertEqualsLog(LogDouble(2.1 / 4.1), ncpd("a2", 'A), 1e-9)
    assertEqualsLog(LogDouble(1.2), ncpd("b1", 'B), 1e-9)
    assertEqualsLog(LogDouble(2.2), ncpd("b2", 'B), 1e-9)
    assertEqualsLog(LogDouble(1.3 / 4.3), ncpd("c1", 'C), 1e-9)
    assertEqualsLog(LogDouble(2.3 / 4.3), ncpd("c2", 'C), 1e-9)

    assertEquals("s", ncpd.sample('S))
    assertEquals("r", ncpd.sample('R))
    assertEquals("t", ncpd.sample('T))
  }

  @Test
  def test_AlphaBetaConditionalLogProbabilityDistribution {
    // throw new NotImplementedError("Test not written") // Covered by test_AlphaBetaConditionalLogProbabilityDistribution_companion
  }

  @Test
  def test_AlphaBetaConditionalLogProbabilityDistribution_companion {
    val mockUnsmoothedCounts = Map[String, Map[Symbol, LogDouble]](
      "a" -> Map(
        'a -> LogDouble(1),
        'b -> LogDouble(2)),
      "b" -> Map(
        'b -> LogDouble(3),
        'c -> LogDouble(4),
        'e -> LogDouble(5)))

    val mockPrior = new ConditionalLogProbabilityDistribution[String, Symbol] {
      def apply(x: Symbol, given: String): LogDouble = (given, x) match {
        case ("a", 'a) => LogDouble(0.11)
        case ("a", 'b) => LogDouble(0.12)
        case ("a", 'c) => LogDouble(0.13)
        case ("a", 'd) => LogDouble(0.14)
        case ("a", 'e) => LogDouble(0.15)

        case ("b", 'a) => LogDouble(0.21)
        case ("b", 'b) => LogDouble(0.22)
        case ("b", 'c) => LogDouble(0.23)
        case ("b", 'd) => LogDouble(0.24)
        case ("b", 'e) => LogDouble(0.25)

        case ("c", 'a) => LogDouble(0.31)
        case ("c", 'b) => LogDouble(0.32)
        case ("c", 'c) => LogDouble(0.33)
        case ("c", 'd) => LogDouble(0.34)
        case ("c", 'e) => LogDouble(0.35)

        case ("d", 'a) => LogDouble(0.41)
        case ("d", 'b) => LogDouble(0.42)
        case ("d", 'c) => LogDouble(0.43)
        case ("d", 'd) => LogDouble(0.44)
        case ("d", 'e) => LogDouble(0.45)
      }
      def sample(given: String): Symbol = ???
    }

    val abcpd = AlphaBetaConditionalLogProbabilityDistribution[String, Symbol](
      mockUnsmoothedCounts,
      alpha = LogDouble(1.1),
      mockPrior,
      knownAs = Some(Set("b", "d")),
      knownBs = Some(Set('c, 'd, 'e)),
      excludedBs = Some(Set('e)),
      totalAddition = LogDouble(10.0))

    assertEqualsLog(LogDouble((1.1 * 0.11 + 1) / (1.1 * 1.0 + 3 + 10)), abcpd('a, "a"), 1e-9)
    assertEqualsLog(LogDouble((1.1 * 0.12 + 2) / (1.1 * 1.0 + 3 + 10)), abcpd('b, "a"), 1e-9)
    assertEqualsLog(LogDouble((1.1 * 0.13 + 0) / (1.1 * 1.0 + 3 + 10)), abcpd('c, "a"), 1e-9)
    assertEqualsLog(LogDouble((1.1 * 0.14 + 0) / (1.1 * 1.0 + 3 + 10)), abcpd('d, "a"), 1e-9)
    assertEqualsLog(LogDouble.zero, /*                               */ abcpd('e, "a"), 1e-9)

    assertEqualsLog(LogDouble((1.1 * 0.21 + 0) / (1.1 * 1.0 + 7 + 10)), abcpd('a, "b"), 1e-9)
    assertEqualsLog(LogDouble((1.1 * 0.22 + 3) / (1.1 * 1.0 + 7 + 10)), abcpd('b, "b"), 1e-9)
    assertEqualsLog(LogDouble((1.1 * 0.23 + 4) / (1.1 * 1.0 + 7 + 10)), abcpd('c, "b"), 1e-9)
    assertEqualsLog(LogDouble((1.1 * 0.24 + 0) / (1.1 * 1.0 + 7 + 10)), abcpd('d, "b"), 1e-9)
    assertEqualsLog(LogDouble.zero, /*                               */ abcpd('e, "b"), 1e-9)

    assertEqualsLog(LogDouble((1.1 * 0.31 + 0) / (1.1 * 1.0 + 0 + 10)), abcpd('a, "c"), 1e-9)
    assertEqualsLog(LogDouble((1.1 * 0.32 + 0) / (1.1 * 1.0 + 0 + 10)), abcpd('b, "c"), 1e-9)
    assertEqualsLog(LogDouble((1.1 * 0.33 + 0) / (1.1 * 1.0 + 0 + 10)), abcpd('c, "c"), 1e-9)
    assertEqualsLog(LogDouble((1.1 * 0.34 + 0) / (1.1 * 1.0 + 0 + 10)), abcpd('d, "c"), 1e-9)
    assertEqualsLog(LogDouble.zero, /*                               */ abcpd('e, "c"), 1e-9)

    assertEqualsLog(LogDouble((1.1 * 0.41 + 0) / (1.1 * 1.0 + 0 + 10)), abcpd('a, "d"), 1e-9)
    assertEqualsLog(LogDouble((1.1 * 0.42 + 0) / (1.1 * 1.0 + 0 + 10)), abcpd('b, "d"), 1e-9)
    assertEqualsLog(LogDouble((1.1 * 0.43 + 0) / (1.1 * 1.0 + 0 + 10)), abcpd('c, "d"), 1e-9)
    assertEqualsLog(LogDouble((1.1 * 0.44 + 0) / (1.1 * 1.0 + 0 + 10)), abcpd('d, "d"), 1e-9)
    assertEqualsLog(LogDouble.zero, /*                               */ abcpd('e, "d"), 1e-9)

    val aSamples = Vector.fill(1000000)(abcpd.sample("a"))
    val aProportions = aSamples.counts.normalizeValues
    assertEquals(4, aProportions.size)
    assertEquals((abcpd('a, "a") / Vector('a, 'b, 'c, 'd).sumBy(abcpd(_, "a"))).toDouble, aProportions('a), 0.01)
    assertEquals((abcpd('b, "a") / Vector('a, 'b, 'c, 'd).sumBy(abcpd(_, "a"))).toDouble, aProportions('b), 0.01)
    assertEquals((abcpd('c, "a") / Vector('a, 'b, 'c, 'd).sumBy(abcpd(_, "a"))).toDouble, aProportions('c), 0.01)
    assertEquals((abcpd('d, "a") / Vector('a, 'b, 'c, 'd).sumBy(abcpd(_, "a"))).toDouble, aProportions('d), 0.01)

    val bSamples = Vector.fill(1000000)(abcpd.sample("b"))
    val bProportions = bSamples.counts.normalizeValues
    assertEquals(4, bProportions.size)
    assertEquals((abcpd('a, "b") / Vector('a, 'b, 'c, 'd).sumBy(abcpd(_, "b"))).toDouble, bProportions('a), 0.01)
    assertEquals((abcpd('b, "b") / Vector('a, 'b, 'c, 'd).sumBy(abcpd(_, "b"))).toDouble, bProportions('b), 0.01)
    assertEquals((abcpd('c, "b") / Vector('a, 'b, 'c, 'd).sumBy(abcpd(_, "b"))).toDouble, bProportions('c), 0.01)
    assertEquals((abcpd('d, "b") / Vector('a, 'b, 'c, 'd).sumBy(abcpd(_, "b"))).toDouble, bProportions('d), 0.01)

    val cSamples = Vector.fill(1000000)(abcpd.sample("c"))
    val cProportions = cSamples.counts.normalizeValues
    assertEquals(4, cProportions.size)
    assertEquals((abcpd('a, "c") / Vector('a, 'b, 'c, 'd).sumBy(abcpd(_, "c"))).toDouble, cProportions('a), 0.01)
    assertEquals((abcpd('b, "c") / Vector('a, 'b, 'c, 'd).sumBy(abcpd(_, "c"))).toDouble, cProportions('b), 0.01)
    assertEquals((abcpd('c, "c") / Vector('a, 'b, 'c, 'd).sumBy(abcpd(_, "c"))).toDouble, cProportions('c), 0.01)
    assertEquals((abcpd('d, "c") / Vector('a, 'b, 'c, 'd).sumBy(abcpd(_, "c"))).toDouble, cProportions('d), 0.01)

    val dSamples = Vector.fill(1000000)(abcpd.sample("d"))
    val dProportions = dSamples.counts.normalizeValues
    assertEquals(4, dProportions.size)
    assertEquals((abcpd('a, "d") / Vector('a, 'b, 'c, 'd).sumBy(abcpd(_, "d"))).toDouble, dProportions('a), 0.01)
    assertEquals((abcpd('b, "d") / Vector('a, 'b, 'c, 'd).sumBy(abcpd(_, "d"))).toDouble, dProportions('b), 0.01)
    assertEquals((abcpd('c, "d") / Vector('a, 'b, 'c, 'd).sumBy(abcpd(_, "d"))).toDouble, dProportions('c), 0.01)
    assertEquals((abcpd('d, "d") / Vector('a, 'b, 'c, 'd).sumBy(abcpd(_, "d"))).toDouble, dProportions('d), 0.01)
  }

  @Test
  def test_AlphaBetaConditionalLogProbabilityDistribution_bugfix {
    val mockUnsmoothedCounts = Map[String, Map[Symbol, LogDouble]](
      "a" -> Map(
        'a -> LogDouble(100),
        'b -> LogDouble(200),
        'c -> LogDouble(300)))

    val mockPrior = new ConditionalLogProbabilityDistribution[String, Symbol] {
      def apply(x: Symbol, given: String): LogDouble = (given, x) match {
        case ("a", 'a) => LogDouble(0.33)
        case ("a", 'b) => LogDouble(0.33)
        case ("a", 'c) => LogDouble(0.33)

        case ("b", 'd) => LogDouble(0.99)

        case ("c", 'e) => LogDouble(0.01)
      }
      def sample(given: String): Symbol = ???
    }

    val abcpd = AlphaBetaConditionalLogProbabilityDistribution[String, Symbol](
      mockUnsmoothedCounts,
      alpha = LogDouble(1.1),
      mockPrior)

    assertEqualsLog(LogDouble((1.1 * 0.33 + 100) / (1.1 * 1.0 + 600)), abcpd('a, "a"), 1e-9)
    assertEqualsLog(LogDouble((1.1 * 0.33 + 200) / (1.1 * 1.0 + 600)), abcpd('b, "a"), 1e-9)
    assertEqualsLog(LogDouble((1.1 * 0.33 + 300) / (1.1 * 1.0 + 600)), abcpd('c, "a"), 1e-9)

    assertEqualsLog(LogDouble((1.1 * 0.99 + 0) / (1.1 * 1.0 + 0)), abcpd('d, "b"), 1e-9)
    assertEqualsLog(LogDouble((1.1 * 0.01 + 0) / (1.1 * 1.0 + 0)), abcpd('e, "c"), 1e-9)
  }

  @Test
  def test_InterpolatingConditionalLogProbabilityDistribution {
    val _a = new ConditionalLogProbabilityDistribution[Symbol, String] {
      def apply(x: String, given: Symbol): LogDouble = (given, x) match {
        case ('a, "1") => LogDouble(0.11)
        case ('b, "2") => LogDouble(0.21)
      }
      def sample(given: Symbol): String = given match {
        case 'a => "aa"
        case 'b => "ab"
      }
    }
    val _b = new ConditionalLogProbabilityDistribution[Symbol, String] {
      def apply(x: String, given: Symbol): LogDouble = (given, x) match {
        case ('a, "1") => LogDouble(0.32)
        case ('b, "2") => LogDouble(0.42)
      }
      def sample(given: Symbol): String = given match {
        case 'a => "ba"
        case 'b => "bb"
      }
    }
    val _c = new ConditionalLogProbabilityDistribution[Symbol, String] {
      def apply(x: String, given: Symbol): LogDouble = (given, x) match {
        case ('a, "1") => LogDouble(0.53)
        case ('b, "2") => LogDouble(0.63)
      }
      def sample(given: Symbol): String = given match {
        case 'a => "ca"
        case 'b => "cb"
      }
    }

    val cpd = new InterpolatingConditionalLogProbabilityDistribution(Vector(
      _a -> LogDouble(0.2),
      _b -> LogDouble(0.3),
      _c -> LogDouble(0.5)))

    assertEqualsLog(LogDouble(0.11 * 0.2 + 0.32 * 0.3 + 0.53 * 0.5), cpd("1", 'a), 1e-9)
    assertEqualsLog(LogDouble(0.21 * 0.2 + 0.42 * 0.3 + 0.63 * 0.5), cpd("2", 'b), 1e-9)

    val asamples = Vector.fill(1000000)(cpd.sample('a))
    val aproportions = asamples.counts.normalizeValues
    assertEquals(3, aproportions.size)
    assertEquals(0.2, aproportions("aa"), 0.01)
    assertEquals(0.3, aproportions("ba"), 0.01)
    assertEquals(0.5, aproportions("ca"), 0.01)

    val bsamples = Vector.fill(1000000)(cpd.sample('b))
    val bproportions = bsamples.counts.normalizeValues
    assertEquals(3, bproportions.size)
    assertEquals(0.2, bproportions("ab"), 0.01)
    assertEquals(0.3, bproportions("bb"), 0.01)
    assertEquals(0.5, bproportions("cb"), 0.01)
  }

  @Test
  def test_ReversingConditionalLogProbabilityDistribution {
    val _cpd = new ConditionalLogProbabilityDistribution[Symbol, Int] {
      def apply(x: Int, given: Symbol): LogDouble = (given, x) match {
        case ('a, 1) => LogDouble(0.5)
        case ('b, 2) => LogDouble(0.1)
        case ('c, 3) => LogDouble(0.7)
      }
      def sample(given: Symbol): Int = ???
    }
    val cpd = new ReversingConditionalLogProbabilityDistribution(_cpd)

    assertEqualsLog(LogDouble(0.5), cpd('a, 1), 1e-9)
    assertEqualsLog(LogDouble(0.1), cpd('b, 2), 1e-9)
    assertEqualsLog(LogDouble(0.7), cpd('c, 3), 1e-9)

    assertException(cpd.sample(1)) { case e: RuntimeException => assertEquals("sample not available on ReversingConditionalLogProbabilityDistribution", e.getMessage) }
  }

  @Test
  def emptyCPD {
    val cpd = ConditionalLogProbabilityDistribution.empty[Symbol, Int]
    assertEqualsLog(LogDouble(0.0), cpd(1, 'z), 1e-9)
    assertException(cpd.sample('z)) { case e: AssertionError => assertEquals("assertion failed: cannot sample from an empty distribution", e.getMessage) }
  }

  def assertEqualsLog(a: LogDouble, b: LogDouble, c: Double) {
    assertEquals(a.toDouble, b.toDouble, c)
  }

}
