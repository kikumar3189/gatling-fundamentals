package finalSimulation

import io.gatling.core.scenario.Simulation
import io.gatling.core.Predef._
import io.gatling.core.structure.ChainBuilder
import io.gatling.http.Predef._

import java.time.LocalDate
import java.time.format.DateTimeFormatter
import scala.concurrent.duration._
import scala.util.Random


class E2ETest extends Simulation{

  //1. Get All Games
  val httpConf = http.baseUrl("http://localhost:8080/app/")
    .header("Accept", "application/json")



  def getProperty(propertyName: String, defaultValue: String): String = {
    Option(System.getenv(propertyName))
      .orElse(Option(System.getProperty(propertyName)))
      .getOrElse(defaultValue)
  }

  def userCount: Int = getProperty("USERS", "3").toInt
  def rampDuration: Int = getProperty("RAMP_DURATION", "10").toInt
  def testDuration: Int = getProperty("DURATION", "60").toInt

  before {
    println(s"Running test with ${userCount} users")
    println(s"Ramping users ${userCount} over seconds ${rampDuration}")
    println(s"Running test for duration ${testDuration}")
  }


  def getAllGames(): ChainBuilder = {
    println("Fetching all video games...")
    exec(http("Get All Video games")
        .get("videogames")
        .check(status.is(200)))
  }

  var idNumbers = (41 to 50).iterator
  val rnd = new Random()
  val now = LocalDate.now()
  val pattern = DateTimeFormatter.ofPattern("yyyy-MM-dd")

  def randomString(length: Int) = {
    rnd.alphanumeric.filter(_.isLetter).take(length).mkString
  }

  def getRandomDate(startDate: LocalDate, random: Random): String = {
    startDate.minusDays(random.nextInt(30)).format(pattern)
  }

  val customFeeder = Iterator.continually(Map(
    "gameId" -> idNumbers.next(),
    "name" -> ("Game-" + randomString(5)),
    "releaseDate" -> getRandomDate(now, rnd),
    "reviewScore" -> rnd.nextInt(100),
    "category" -> ("Category-" + randomString(6)),
    "rating" -> ("Rating-" + randomString(4))
  ))

  //2. Create a new game (Use a custom feeder)
  def postNewGame() = {
      feed(customFeeder)
        .exec(http("Post New Game with ID ${gameId}")
          .post("videogames/")
          .body(ElFileBody("bodies/NewGameTemplate.json")).asJson
          .check(status.is(200))

        )
        .pause(1)

  }

  //3. Get Single game (Game created above)
  def getSingleGame(): ChainBuilder = {
    print("Fetching single game ")
      exec(http("Get single video game with ID ${gameId}")
        .get("videogames/${gameId}")
        .check(status.is(200))
        .check(jsonPath("$.name").is("${name}"))
      )
  }



  //4. Delete the game just create

  def deleteSingleGame(): ChainBuilder = {
    exec(http("Delete single video game with ID ${gameId}")
        .delete("videogames/${gameId}")
        .check(status.is(200)))
  }

  val scn = scenario("Video game e2e test")
    .exec(getAllGames())
    .pause(2)
    .exec(postNewGame())
    .pause(2)
    .exec(getSingleGame())
    .pause(2)
    .exec(deleteSingleGame())


  setUp(
    scn.inject(
      nothingFor(5.seconds),
      rampUsers(userCount) during(rampDuration.second)
    )
  ).protocols(httpConf)
    .maxDuration(testDuration.second)

}
