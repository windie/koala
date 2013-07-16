package me.iamzsx.wikimath

import java.io._

import scala.annotation.meta.field
import scala.collection.mutable.ArrayBuffer

import org.mortbay.jetty.Server
import org.mortbay.jetty.handler.HandlerList
import org.mortbay.jetty.handler.ResourceHandler
import org.mortbay.jetty.nio.SelectChannelConnector
import org.mortbay.jetty.servlet.ServletHandler
import org.mortbay.jetty.servlet.ServletHolder
import org.mortbay.jetty.servlet.ServletMapping
import org.mortbay.thread.QueuedThreadPool

import javax.servlet.http.HttpServlet
import javax.servlet.http.HttpServletRequest
import javax.servlet.http.HttpServletResponse
import play.api.libs.json.Json

class HttpServer {
  val webServer = new Server

  val holders = ArrayBuffer[ServletHolder]();
  val mappings = ArrayBuffer[ServletMapping]();

  init

  def init {
    val connector = new SelectChannelConnector
    connector.setPort(Config.get.getInt("webserver.port"))
    webServer.addConnector(connector)
    
    webServer.setThreadPool(new QueuedThreadPool)

    val mainHandler = new HandlerList

    val resourceHandler = new ResourceHandler()
    resourceHandler.setResourceBase("webapp/")
    mainHandler.addHandler(resourceHandler)

    val servletHandler = new ServletHandler

    registerServlet(classOf[SearchServlet], "/search")

    servletHandler.setServlets(holders.toArray)
    servletHandler.setServletMappings(mappings.toArray)

    mainHandler.addHandler(servletHandler)

    webServer.setHandler(mainHandler)

  }

  private def registerServlet(servlet: Class[_], path: String) {
    val holder = new ServletHolder()
    holder.setName(servlet.getName())
    holder.setClassName(servlet.getName())
    holders += holder

    val mapping = new ServletMapping;
    mapping.setPathSpec(path)
    mapping.setServletName(servlet.getName())
    mappings += mapping
  }

  def start {
    webServer.start
  }
}

object HttpServer {

  def main(args: Array[String]) {
    val server = new HttpServer;
    server.start
  }
}

class SearchServlet extends HttpServlet {
  override def doGet(req: HttpServletRequest, resp: HttpServletResponse) {
    resp.setContentType("application/json")

    val query = req.getParameter("q")
    if (query == null || query.isEmpty()) {
      resp.getWriter().print("""{ "status": "need query parameter" }""")
      resp.getWriter().println()
      return ;
    }

    var page = req.getParameter("page")
    if (page == null) {
      page = "0"
    }

    var pageSize = req.getParameter("page_size")
    if (pageSize == null) {
      pageSize = "10"
    }

    try {
      val json = FormulaSearcher.search(query, page.toInt, pageSize.toInt)
      resp.getWriter().print(Json.stringify(json))
      resp.getWriter().println()
    } catch {
      case e: Exception => {
        e.printStackTrace
        resp.getWriter().print("""{ "status": """" + e.getMessage + """" }""")
        resp.getWriter().println()
      }
    }
  }
}

