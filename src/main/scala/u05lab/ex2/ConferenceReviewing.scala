package u05lab.ex2

trait ConferenceReviewing:

  /**
   * loads a review for the specified article, with complete scores as a map
   *
   * @param article to load
   * @param scores  scores of the article
   */
  def loadReview(article: Int, scores: Map[Question, Int]): Unit

  /**
   * loads a review for the specified article, with the 4 explicit scores
   *
   * @param article      to load
   * @param relevance    score
   * @param significance score
   * @param confidence   score
   * @param fin          score
   */
  def loadReview(article: Int, relevance: Int, significance: Int, confidence: Int, fin: Int): Unit

  /**
   * @param article  to get
   * @param question to get score
   * @return the scores given to the specified article and specified question, as an (ascending-ordered) list
   */
  def orderedScores(article: Int, question: Question): List[Int]

  /**
   * @param article to get
   * @return the average score to question FINAL taken by the specified article
   */
  def averageFinalScore(article: Int): Double

  /**
   * An article is considered accept if its averageFinalScore (not weighted) is > 5, and at least one RELEVANCE score that is >= 8.
   *
   * @return the set of accepted articles
   */
  def acceptedArticles: Set[Int]


  /**
   * @return accepted articles as a list of pairs article+averageFinalScore, ordered from worst to best based on averageFinalScore
   */
  def sortedAcceptedArticles: List[(Int, Double)]

  /**
   * @return a map from articles to their average "weighted final score", namely, the average value of CONFIDENCE*FINAL/10
   */
  def averageWeightedFinalScoreMap: Map[Int, Double]

enum Question:
  case RELEVANCE, SIGNIFICANCE, CONFIDENCE, FINAL

object ConferenceReviewing:
  def apply(): ConferenceReviewing = ConferenceReviewingImpl()

  private class ConferenceReviewingImpl extends ConferenceReviewing:
    override def loadReview(article: Int, relevance: Int, significance: Int, confidence: Int, fin: Int): Unit = ???

    override def orderedScores(article: Int, question: Question): List[Int] = ???

    override def averageFinalScore(article: Int): Double = ???

    override def acceptedArticles: Set[Int] = ???

    override def sortedAcceptedArticles: List[(Int, Double)] = ???

    override def averageWeightedFinalScoreMap: Map[Int, Double] = ???

    override def loadReview(article: Int, scores: Map[Question, Int]): Unit = ???


