package exercises.action.fp.search

import exercises.action.DateGenerator._
import exercises.action.fp.IO
import exercises.action.fp.search.Airport._
import exercises.action.fp.search.SearchFlightGenerator._
import org.scalacheck.{Arbitrary, Gen}
import org.scalacheck.Arbitrary.arbitrary
import org.scalatest.funsuite.AnyFunSuite
import org.scalatestplus.scalacheck.ScalaCheckDrivenPropertyChecks

import java.time.{Duration, Instant, LocalDate}
import scala.concurrent.ExecutionContext
import scala.util.Random

// Run the test using the green arrow next to class name (if using IntelliJ)
// or run `sbt` in the terminal to open it in shell mode, then type:
// testOnly exercises.action.fp.search.SearchFlightServiceTest
class SearchFlightServiceTest extends AnyFunSuite with ScalaCheckDrivenPropertyChecks {

  implicit val ec: scala.concurrent.ExecutionContext = scala.concurrent.ExecutionContext.global

  test("fromTwoClients example") {
    val now   = Instant.now()
    val today = LocalDate.now()

    val flight1 = Flight("1", "BA", parisOrly, londonGatwick, now, Duration.ofMinutes(100), 0, 89.5, "")
    val flight2 = Flight("2", "LH", parisOrly, londonGatwick, now, Duration.ofMinutes(105), 0, 96.5, "")
    val flight3 = Flight("3", "BA", parisOrly, londonGatwick, now, Duration.ofMinutes(140), 1, 234.0, "")
    val flight4 = Flight("4", "LH", parisOrly, londonGatwick, now, Duration.ofMinutes(210), 2, 55.5, "")

    val client1 = SearchFlightClient.constant(IO(List(flight3, flight1)))
    val client2 = SearchFlightClient.constant(IO(List(flight2, flight4)))

    val service = SearchFlightService.fromTwoClients(client1, client2)
    val result  = service.search(parisOrly, londonGatwick, today).unsafeRun()

    assert(result == SearchResult(List(flight1, flight2, flight3, flight4)))
  }

  test("fromTwoClients error") {
    val now   = Instant.now()
    val today = LocalDate.now()

    val flight1 = Flight("1", "BA", parisOrly, londonGatwick, now, Duration.ofMinutes(100), 0, 89.5, "")

    val client1 = SearchFlightClient.constant(IO(List(flight1)))
    val client2 = SearchFlightClient.constant(IO.fail(new Exception("Client failed")))

    val service = SearchFlightService.fromTwoClients(client1, client2)
    val result  = service.search(parisOrly, londonGatwick, today).attempt.unsafeRun()

    assert(result.isSuccess)
  }

  test("fromTwoClients error property") {
    forAll(Gen.listOf(flightGen), Gen.listOf(flightGen), arbitrary[Boolean], arbitrary[Boolean]) {
      (l1: List[Flight], l2: List[Flight], isError1: Boolean, isError2: Boolean) =>
        val now        = Instant.now()
        val today      = LocalDate.now()
        val clientFail = SearchFlightClient.constant(IO.fail(new Exception("Client failed")))

        val l1Unique   = l1.distinctBy(_.flightId)
        val l2Unique   = l2.distinctBy(_.flightId)
        val bothUnique = (l1 ++ l2).distinctBy(_.flightId)

        val client1 = if (isError1) clientFail else SearchFlightClient.constant(IO(l1))
        val client2 = if (isError2) clientFail else SearchFlightClient.constant(IO(l2))

        val service = SearchFlightService.fromTwoClients(client1, client2)
        val result  = service.search(parisOrly, londonGatwick, today).unsafeRun()

        (isError1, isError2) match {
          case (false, false) => assert(result.flights.length == bothUnique.length)
          case (false, true)  => assert(result.flights.length == l1Unique.length)
          case (true, false)  => assert(result.flights.length == l2Unique.length)
          case (true, true)   => assert(result.flights.length == 0)
        }
    }
  }

  test("fromClients") {
    val now   = Instant.now()
    val today = LocalDate.now()

    val flight1 = Flight("1", "BA", parisOrly, londonGatwick, now, Duration.ofMinutes(100), 0, 89.5, "")
    val flight2 = Flight("2", "LH", parisOrly, londonGatwick, now, Duration.ofMinutes(105), 0, 96.5, "")
    val flight3 = Flight("3", "BA", parisOrly, londonGatwick, now, Duration.ofMinutes(140), 1, 234.0, "")
    val flight4 = Flight("4", "LH", parisOrly, londonGatwick, now, Duration.ofMinutes(210), 2, 55.5, "")

    val client1 = SearchFlightClient.constant(IO(List(flight1)))
    val client2 = SearchFlightClient.constant(IO(List(flight2)))
    val client3 = SearchFlightClient.constant(IO(List(flight3)))
    val client4 = SearchFlightClient.constant(IO(List(flight4)))

    val service = SearchFlightService.fromClients(List(client1, client2, client3, client4))
    val result  = service.search(parisOrly, londonGatwick, today).unsafeRun()

    assert(result == SearchResult(List(flight1, flight2, flight3, flight4)))
  }

  test("fromClients error property") {
    forAll(arbitrary[LocalDate], Gen.listOf(clientGen)) {
      (today: LocalDate, clients: List[SearchFlightClient]) =>

        val service = SearchFlightService.fromClients(clients)
        val result  = service.search(parisOrly, londonGatwick, today).attempt.unsafeRun()

        assert(result.isSuccess)
    }
  }

  test("clientt' order doesn't matter") {
    forAll(airportGen, airportGen, arbitrary[LocalDate], Gen.listOf(clientGen)) {
      (from: Airport, to: Airport, today: LocalDate, clients: List[SearchFlightClient]) =>

        val shuffled = Random.shuffle(clients)

        val service1 = SearchFlightService.fromClients(clients)
        val service2 = SearchFlightService.fromClients(shuffled)

        val result1  = service1.search(from, to, today).unsafeRun()
        val result2  = service2.search(from, to, today).unsafeRun()

        assert(result1 == result2)
    }
  }

}
