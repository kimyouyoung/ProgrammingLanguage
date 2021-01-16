package edu.handong.csee.plt.ast;

public class Fun extends AST{
	
	String param = "";
	AST body = new AST();
	
	public Fun(String param, AST body) {
		this.param = param;
		this.body = body;
	}
	
	public String getParam() {
		return param;
	}
	
	public AST getBody() {
		return body;
	}

	public String getASTCode() {
		return "(fun '" + param + " " + body.getASTCode() + ")";
	}
}
