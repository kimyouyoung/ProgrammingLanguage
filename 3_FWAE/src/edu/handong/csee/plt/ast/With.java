package edu.handong.csee.plt.ast;

public class With extends AST{
	String name = "";
	AST value = new AST();
	AST body = new AST();
	
	public With(String name, AST value, AST body) {
		this.name = name;
		this.value = value;
		this.body = body;
	}
	
	public String getName() {
		return name;
	}

	public AST getValue() {
		return value;
	}
	
	public AST getBody() {
		return body;
	}

	public String getASTCode() {
		return "(with '" + name + " " + value.getASTCode() + " " + body.getASTCode() + ")";
	}
}