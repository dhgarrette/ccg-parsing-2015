package dhg.ccg.prob

import dhg.util._
import org.junit.Test
import org.junit.Assert._

class ExpProbabilityDistributionTests {

  @Test
  def simplePD {
    val pd = new SimpleExpProbabilityDistribution(Map('a -> 1.0, 'b -> 2.0, 'c -> 7.0))
    assertEquals(0.1, pd('a), 1e-9)
    assertEquals(0.2, pd('b), 1e-9)
    assertEquals(0.7, pd('c), 1e-9)
    assertEquals(0.0, pd('z), 1e-9)
    
    val samples = Vector.fill(1000000)(pd.sample())
    val proportions = samples.counts.normalizeValues
    assertEquals(3, proportions.size)
    assertEquals(0.1, proportions('a), 0.001)
    assertEquals(0.2, proportions('b), 0.001)
    assertEquals(0.7, proportions('c), 0.001)
  }
  
  @Test
  def laplacePD {
    val pd = new LaplaceExpProbabilityDistribution(Map('a -> 1.0, 'b -> 2.0, 'c -> 4.0), None, None, 1.0, 0.0)
    assertEquals(0.2, pd('a), 1e-9)
    assertEquals(0.3, pd('b), 1e-9)
    assertEquals(0.5, pd('c), 1e-9)
    assertEquals(0.1, pd('z), 1e-9)
    
    val samples = Vector.fill(1000000)(pd.sample())
    val proportions = samples.counts.normalizeValues
    assertEquals(3, proportions.size)
    assertEquals(0.2, proportions('a), 0.001)
    assertEquals(0.3, proportions('b), 0.001)
    assertEquals(0.5, proportions('c), 0.001)
  }

  @Test
  def laplacePD_totalAddition {
    val pd = new LaplaceExpProbabilityDistribution(Map('a -> 1.0, 'b -> 2.0, 'c -> 4.0), None, None, 1.0, 10.0)
    assertEquals(0.10, pd('a), 1e-9)
    assertEquals(0.15, pd('b), 1e-9)
    assertEquals(0.25, pd('c), 1e-9)
    assertEquals(0.05, pd('z), 1e-9)
    
    val samples = Vector.fill(1000000)(pd.sample())
    val proportions = samples.counts.normalizeValues
    assertEquals(3, proportions.size)
    assertEquals(0.2, proportions('a), 0.001)
    assertEquals(0.3, proportions('b), 0.001)
    assertEquals(0.5, proportions('c), 0.001)
  }

}
