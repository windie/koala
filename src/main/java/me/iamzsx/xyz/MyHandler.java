package me.iamzsx.xyz;

import org.xml.sax.Attributes;
import org.xml.sax.ContentHandler;
import org.xml.sax.ErrorHandler;
import org.xml.sax.Locator;
import org.xml.sax.SAXException;
import org.xml.sax.SAXParseException;

public class MyHandler implements ContentHandler, ErrorHandler {
	
	public void clear() {
	}

	@Override
	public void characters(char[] ch, int start, int length)
			throws SAXException {
		System.out.println("w:" + new String(ch, start, length));
	}

	@Override
	public void endDocument() throws SAXException {
	}

	@Override
	public void endElement(String uri, String name, String qName)
			throws SAXException {
		System.out.println("c:" + qName);
	}

	@Override
	public void endPrefixMapping(String arg0) throws SAXException {

	}

	@Override
	public void ignorableWhitespace(char[] ch, int start, int length)
			throws SAXException {

	}

	@Override
	public void processingInstruction(String target, String data)
			throws SAXException {
	}

	@Override
	public void setDocumentLocator(Locator locator) {
	}

	@Override
	public void skippedEntity(String name) throws SAXException {
		System.out.println("e: " + name );
	}

	@Override
	public void startDocument() throws SAXException {
	}

	@Override
	public void startElement(String uri, String name, String qName,
			Attributes attrs) throws SAXException {
		System.out.println("n: " + name + " s:" + qName);
	}

	@Override
	public void startPrefixMapping(String prefix, String uri)
			throws SAXException {
	}

	@Override
	public void error(SAXParseException expection) throws SAXException {
	}

	@Override
	public void fatalError(SAXParseException expection) throws SAXException {
	}

	@Override
	public void warning(SAXParseException expection) throws SAXException {
	}

}
