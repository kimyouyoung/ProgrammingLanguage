package edu.handong.csee.plt.ast;

public class OpenBox extends AST{
	
	AST val = new AST();
	
	public OpenBox(AST val) {
		this.val = val;
	}

	public AST getVal() {
		return val;
	}

	public String getASTCode() {
		return "(openbox " + " " + val.getASTCode() + ")";
	}

}
