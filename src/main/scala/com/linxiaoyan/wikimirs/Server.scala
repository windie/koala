package com.linxiaoyan.wikimirs

import org.glassfish.jersey.server.ResourceConfig
import org.glassfish.jersey.servlet.ServletContainer
import org.mortbay.jetty.Handler
import org.mortbay.jetty.Server
import org.mortbay.jetty.handler.HandlerList
import org.mortbay.jetty.handler.ResourceHandler
import org.mortbay.jetty.nio.SelectChannelConnector
import org.mortbay.jetty.servlet.Context
import org.mortbay.jetty.servlet.ServletHolder
import org.mortbay.log.Log
import org.mortbay.thread.QueuedThreadPool
import javax.ws.rs.DELETE
import javax.ws.rs.DefaultValue
import javax.ws.rs.GET
import javax.ws.rs.PUT
import javax.ws.rs.Path
import javax.ws.rs.PathParam
import javax.ws.rs.Produces
import javax.ws.rs.QueryParam
import play.api.libs.json.Json
import play.api.libs.json.Json.toJsFieldJsValueWrapper
import javax.ws.rs.core.MediaType
import com.typesafe.scalalogging.slf4j.Logging
import play.api.libs.json.JsArray
import scala.math._
import play.api.libs.json._
import play.api.libs.functional.syntax._

trait Service extends Logging {
  protected def handleError(message: String): String = {
    Json.stringify(Json.obj(
      "status" -> "Fail",
      "message" -> message))
  }

  protected def handleError(e: Throwable): String = {
    logger.error(e.getMessage, e)
    handleError(e.getMessage)
  }
}

@Path("/api/dcg")
class DCGService extends Service {

  case class QueryDCG(query: String, dcg: Double, active: Int, precision: Double)
  implicit val queryDCGWrites = Json.writes[QueryDCG]

  case class DCGResult(dcg: Double, precision: Double, details: Seq[QueryDCG])
  implicit val dcgResultWrites = Json.writes[DCGResult]

  @GET
  @Path("{position}")
  @Produces(Array(MediaType.APPLICATION_JSON))
  def get(@PathParam("position") rankPosition: Int): String = {
    val queries = LabelStorager().allQueries
    val dcgs = queries.map(query => {
      val (dcg, active, relevanceCount) = calcDCG(query, rankPosition)
      QueryDCG(query, dcg, active, relevanceCount.toDouble / rankPosition)
    })
    val averageDCG = dcgs.map(_.dcg).sum / dcgs.length
    val averagePrecision = dcgs.map(_.precision).sum / dcgs.length
    Json.prettyPrint(Json.toJson(DCGResult(averageDCG, averagePrecision, dcgs)))
  }

  private[this] def calcDCG(query: String, rankPosition: Int): (Double, Int, Int) = {
    val json = FormulaSearcher.search(query, 1, rankPosition)
    val relevances = (json \ "results").asInstanceOf[JsArray].value.map({
      o =>
        (o \ "doc" \ "doc_url").as[String]
    })
      .map(p => LabelStorager().get(new LabelUnit(query, p)))
    val active = relevances.count(_ > 0)
    val dcg = relevances.zipWithIndex.map({
      case (r, i) =>
        val p = i + 1
        pow(2.0, r) - 1.0 / (log(p + 1.0) / log(2.0))
    }).sum
    val relevanceCount = relevances.count(_ > 1)
    (dcg, active, relevanceCount)
  }
}

@Path("/api/label")
class LabelService extends Service {

  @PUT
  @Path("{query}/{url}/{label}")
  @Produces(Array(MediaType.APPLICATION_JSON))
  def put(@PathParam("query") query: String,
    @PathParam("url") url: String,
    @PathParam("label")@DefaultValue("0") label: Int): String = {
    logger.debug(s"put: ${query}, ${url}, ${label}")

    try {
      LabelStorager().put(new LabelUnit(query, url), label)
      """{
    "status": "OK"
}
"""
    } catch {
      case e: Throwable => {
        handleError(e.getMessage)
      }
    }
  }

  @GET
  @Path("{query}/{url}")
  @Produces(Array(MediaType.APPLICATION_JSON))
  def get(@PathParam("query") query: String,
    @PathParam("url") url: String): String = {
    logger.debug(s"get: ${query}, ${url}")

    try {
      val label = LabelStorager().get(new LabelUnit(query, url))
      Json.stringify(Json.obj(
        "status" -> "OK",
        "label" -> label))
    } catch {
      case e: Throwable => {
        handleError(e.getMessage)
      }
    }
  }

  @DELETE
  @Path("{query}/{url}")
  @Produces(Array(MediaType.APPLICATION_JSON))
  def delete(@PathParam("query") query: String,
    @PathParam("url") url: String): String = {
    logger.debug(s"delete: ${query}, ${url}")

    try {
      LabelStorager().put(new LabelUnit(query, url), 0)
      Json.stringify(Json.obj(
        "status" -> "OK"))
    } catch {
      case e: Throwable => {
        handleError(e.getMessage)
      }
    }
  }
}

@Path("/api/search")
class SearchService extends Service {

  @GET
  @Path("{query}/{page}")
  @Produces(Array(MediaType.APPLICATION_JSON))
  def search(@PathParam("query") query: String, @PathParam("page")@DefaultValue("1") page: Int, @QueryParam("size")@DefaultValue("10") pageSize: Int): String = {
    logger.debug(s"request: ${query}, ${page}, ${pageSize}")

    if (query == null || query.isEmpty()) {
      return handleError("need query parameter")
    }

    try {
      val json = com.linxiaoyan.wikimirs.lab.LabSearcher.search(query, page, pageSize)
      Json.stringify(json)
    } catch {
      case e: Throwable => {
        handleError(e)
      }
    }
  }

}

class HttpServer(port: Int) {
  private val webServer = new Server

  init

  private[this] def init {
    val connector = new SelectChannelConnector
    connector.setPort(port)
    webServer.addConnector(connector)
    webServer.setThreadPool(new QueuedThreadPool)

    val mainHandler = new HandlerList
    mainHandler.addHandler(createResouceHandler)
    mainHandler.addHandler(createRESTfulHandler(mainHandler))

    webServer.addHandler(mainHandler)
  }

  private[this] def createResouceHandler: Handler = {
    val resourceHandler = new ResourceHandler()
    resourceHandler.setResourceBase("webapp/")
    resourceHandler
  }

  private[this] def createRESTfulHandler(mainHandler: HandlerList): Handler = {
    val handler = new Context(mainHandler, "/", Context.SESSIONS);
    val config = new ResourceConfig().packages(classOf[SearchService].getPackage().getName())
    val holder = new ServletHolder(new ServletContainer(config))
    handler.addServlet(holder, "/")
    handler
  }

  def start {
    webServer.start
  }
}

object HttpServer {
  def main(args: Array[String]) {
    val server = new HttpServer(Settings.getInt("webserver.port"))
    server.start
    Thread.sleep(2000)
    // Force to warm up the server
    Perf.getQueryTime("a + b")
  }
}
