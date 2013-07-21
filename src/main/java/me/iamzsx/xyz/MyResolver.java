package me.iamzsx.xyz;

import org.xml.sax.EntityResolver;
import org.xml.sax.InputSource;

public class MyResolver implements EntityResolver {
	public InputSource resolveEntity(String publicId, String systemId) {
		System.out.println("s:" + systemId);
		System.out.println("p:" + publicId);
		return null;
	}
}