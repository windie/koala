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

import javax.ws.rs.core.MediaType
import javax.ws.rs.DefaultValue
import javax.ws.rs.GET
import javax.ws.rs.Path
import javax.ws.rs.PathParam
import javax.ws.rs.Produces
import javax.ws.rs.QueryParam

import play.api.libs.json.Json

@Path("/api/search")
class SearchService {

  @GET
  @Path("{query}/{page}")
  @Produces(Array(MediaType.APPLICATION_JSON))
  def search(@PathParam("query") query: String, @PathParam("page")@DefaultValue("1") page: Int, @QueryParam("size")@DefaultValue("10") pageSize: Int): String = {
    if (query == null || query.isEmpty()) {
      return error("need query parameter")
    }

    try {
      val json = FormulaSearcher.search(query, page, pageSize)
      Json.stringify(json)
    } catch {
      case e: Throwable => {
        error(e.getMessage)
      }
    }
  }

  private[this] def error(message: String): String = {
    s"""{
    "status": "${message}"
}
"""
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
    Log.getLog().setDebugEnabled(true)
    val server = new HttpServer(Settings.getInt("webserver.port"));
    server.start
  }
}
