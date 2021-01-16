package edu.handong.csee.plt.ast;

public class SetBox extends AST{
	
	AST bn = new AST();
	AST val = new AST();
	
	public SetBox(AST bn, AST val) {
		this.bn = bn;
		this.val = val;
	}
	
	public AST getBn() {
		return bn;
	}

	public AST getVal() {
		return val;
	}

	public String getASTCode() {
		return "(setbox " + bn.getASTCode() + " " + val.getASTCode() + ")";
	}
	
}
