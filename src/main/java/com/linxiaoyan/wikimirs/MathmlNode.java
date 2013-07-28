package com.linxiaoyan.wikimirs;

public interface MathmlNode extends Iterable<MathmlNode> {

	public boolean isText();
	
	public boolean isLeaf();
	
	public String getLabel();
	
	public MathmlTag getParent();
}
