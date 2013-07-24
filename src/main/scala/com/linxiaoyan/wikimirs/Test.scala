package com.linxiaoyan.wikimirs

import java.io.StringReader
import org.apache.xerces.parsers.SAXParser
import org.xml.sax.InputSource
import me.iamzsx.xyz.MyHandler
import scala.collection.mutable.ListBuffer

object Test extends Application {

  var xml = new SAXParser;
  xml.setFeature("http://xml.org/sax/features/validation", false);
  xml.setFeature("http://apache.org/xml/features/nonvalidating/load-external-dtd", false)
  xml.setFeature("http://xml.org/sax/features/namespaces", false)
  xml.setFeature("http://apache.org/xml/features/validation/unparsed-entity-checking", false)
  xml.setFeature("http://apache.org/xml/features/continue-after-fatal-error", true)
  val handler = new MathmlXMLHandler
  xml.setContentHandler(handler);
  xml.setErrorHandler(handler);
  // xml.setEntityResolver(new MyResolver);
  handler.clear
  xml.parse(new InputSource(new StringReader("""<math><mo>&shortparallel;</mo></math>""")))

  val tokens = ListBuffer[FormulaTerm]()
  handler.toTokens(handler.node.children(0), 1, tokens)
  println(tokens)
  //  val xmlInputFactory = XMLInputFactory.newInstance()
  //  println(xmlInputFactory)
  //  xmlInputFactory.setXMLResolver(new XMLResolver() {
  //    def resolveEntity(publicID: String, systemID: String, baseURI: String, namespace: String): AnyRef = {
  //      println("publicID: " + publicID)
  //      println("systemID: " + systemID)
  //      println("baseURI: " + baseURI)
  //      println("namespace: " + namespace)
  //      null
  //    }
  //  })
  //  xmlInputFactory.setXMLReporter(new XMLReporter() {
  //    def report(message: String, errorType: String, relatedInformation: AnyRef, location: Location) {
  //    	println(message)
  //    }
  //  });
  //
  //  xmlInputFactory.setProperty(XMLInputFactory.IS_COALESCING, false)
  //  xmlInputFactory.setProperty(XMLInputFactory.IS_REPLACING_ENTITY_REFERENCES, false);
  //  xmlInputFactory.setProperty(XMLInputFactory.IS_SUPPORTING_EXTERNAL_ENTITIES, false);
  //
  //  val streamReader = xmlInputFactory.createXMLStreamReader(new StringReader("<math><mo open='&shortparallel;'>1</mo></math>"))
  //  while (streamReader.hasNext()) {
  //    streamReader.next() match {
  //      case XMLStreamConstants.START_ELEMENT =>
  //        println("s:" + streamReader.getLocalName());
  //      case XMLStreamConstants.CHARACTERS =>
  //        println("t:" + streamReader.getText());
  //      case XMLStreamConstants.ENTITY_REFERENCE =>
  //        println("R:" + streamReader.getLocalName());
  //      case XMLStreamConstants.END_ELEMENT =>
  //        println("END:" + streamReader.getLocalName());
  //      case _ =>
  //    }
  //  }

}