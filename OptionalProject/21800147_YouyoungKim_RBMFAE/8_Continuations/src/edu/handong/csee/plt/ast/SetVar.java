package edu.handong.csee.plt.ast;

public class SetVar extends AST{
	
	String id = "";
	AST update = new AST();
	
	public SetVar(String id, AST update) {
		this.id = id;
		this.update = update;
	}
	
	public String getId() {
		return id;
	}

	public AST getUpdate() {
		return update;
	}

	public String getASTCode() {
		return "(setvar '" + id + " " + update.getASTCode() + ")";
	}
	
}
