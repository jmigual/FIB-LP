#header
<<
#include <string>
#include <iostream>
#include <map>
using namespace std;

// struct to store information about tokens
typedef struct {
  string kind;
  string text;
} Attrib;

// function to fill token information (predeclaration)
void zzcr_attr(Attrib *attr, int type, char *text);

// fields for AST nodes
#define AST_FIELDS string kind; string text;
#include "ast.h"

// macro to create a new AST node (and function predeclaration)
#define zzcr_ast(as,attr,ttype,textt) as=createASTnode(attr,ttype,textt)
AST* createASTnode(Attrib* attr,int ttype, char *textt);
>>

<<
#include <cstdlib>
#include <cmath>

//global structures
AST *root;


// function to fill token information
void zzcr_attr(Attrib *attr, int type, char *text) {
/*  if (type == ID) {
    attr->kind = "id";
    attr->text = text;
  }
  else {*/
    attr->kind = text;
    attr->text = "";
//  }
}

// function to create a new AST node
AST* createASTnode(Attrib* attr, int type, char* text) {
  AST* as = new AST;
  as->kind = attr->kind; 
  as->text = attr->text;
  as->right = NULL; 
  as->down = NULL;
  return as;
}


/// create a new "list" AST node with one element
AST* createASTlist(AST *child) {
 AST *as=new AST;
 as->kind="list";
 as->right=NULL;
 as->down=child;
 return as;
}

/// get nth child of a tree. Count starts at 0.
/// if no such child, returns NULL
AST* child(AST *a,int n) {
AST *c=a->down;
for (int i=0; c!=NULL && i<n; i++) c=c->right;
return c;
}



/// print AST, recursively, with indentation
void ASTPrintIndent(AST *a,string s)
{
  if (a==NULL) return;

  cout<<a->kind;
  if (a->text!="") cout<<"("<<a->text<<")";
  cout<<endl;

  AST *i = a->down;
  while (i!=NULL && i->right!=NULL) {
    cout<<s+"  \\__";
    ASTPrintIndent(i,s+"  |"+string(i->kind.size()+i->text.size(),' '));
    i=i->right;
  }
  
  if (i!=NULL) {
      cout<<s+"  \\__";
      ASTPrintIndent(i,s+"   "+string(i->kind.size()+i->text.size(),' '));
      i=i->right;
  }
}

/// print AST 
void ASTPrint(AST *a)
{
  while (a!=NULL) {
    cout<<" ";
    ASTPrintIndent(a,"");
    a=a->right;
  }
}


bool calc_difference(AST *a1, AST *a2) {

}

int calc_critical(AST *a) {
  
  
}

int critical(string role) {

}

bool difference(string role1, string role2) {
  
}

void recorre(AST *a) {
  while (a!=NULL) {
    if (a->kind == "critical") {
      cout << "Critical " << child(a,0)->kind << " : " << critical(child(a,0)->kind) << endl;
    }
    else if (a->kind == "difference") {
      cout << "Difference: " << child(a,0)->kind << " and " << child(a,1)->kind<< " : " << difference(child(a,0)->kind,child(a,1)->kind) << endl;
    }
    a=a->right;
  }  
}



int main() {
  root = NULL;
  ANTLR(bpmn(&root), stdin);
  ASTPrint(root);
  recorre(child(child(root,1),0));
}
>>

#lexclass START
#token STARTP "start"
#token ENDP "end"
#token CONN "connection"
#token FILECONN "file"
#token CRIT "critical"
#token DIFFER "difference"
#token CORRECTF "correctfile"
#token FILEREAD "\->"
#token FILEWRITE "<\-"
#token OPENP "\("
#token CLOSEP "\)"
#token QUERIES "queries"
#token GPAR "\+"
#token GOR "\|"
#token GXOR "\#"
#token SEQ ";"
#token ID "[a-zA-Z][a-zA-Z0-9]*"
#token SPACE "[\ \n]" << zzskip();>>


bpmn: process QUERIES! queries <<#0=createASTlist(_sibling);>>;
process: (rol|rel)* <<#0=createASTlist(_sibling);>>;
queries: (critical|diff|correct)* <<#0=createASTlist(_sibling);>>;

// Process
rol: STARTP! pPar ENDP! ID^;
rel: connection | file;

connection: CONN^ ID ID;
file: FILECONN^ fileType;
fileType: ID (FILEREAD^ | FILEWRITE^) ID;


pSeq: basic (SEQ^ basic)*;
pOr: pSeq (GOR^ pSeq)*;
pXor: pOr (GXOR^ pOr)*;
pPar: pXor (GPAR^ pXor)*;

basic: ID | (OPENP! pPar CLOSEP!);

// Queries
critical: CRIT^ ID;
diff: DIFFER^ ID ID;
correct: CORRECTF^ ID;


