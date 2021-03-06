/* --- Translations object -------------------------------------------------- */

var tree_icon="../minibar/tree-btn.png";
var alignment_icon="../minibar/align-btn.png";

function Translations(server,opts) {
    this.server=server;

    // Default values for options:
    this.options={
	show_abstract: false,
	abstract_action: null, // action when selecting the abstracy syntax tree
	show_trees: false, // add buttons to show abstract syntax trees,
	                   // parse trees & word alignment
	tree_img_format: "png", // format for trees & alignment images,
	                        // can be "gif", "png" or "svg"
	show_grouped_translations: true,
	show_brackets: false, // show bracketed string
	translate_limit: 25 // maximum number of parse trees to retrieve
    }

    // Apply supplied options
    if(opts) for(var o in opts) this.options[o]=opts[o];

    this.main=empty("div");
    this.menus=empty("span");

    this.to_menu=empty_id("select","to_menu");

    appendChildren(this.menus,[text(" To: "), this.to_menu])
    this.to_menu.onchange=bind(this.get_translations,this);

}

Translations.prototype.change_grammar=function(grammar) {
    this.grammar=grammar;
    
    update_language_menu(this.to_menu,grammar);
    insertFirst(this.to_menu,option("All","All"));
    this.to_menu.value="All";
}

Translations.prototype.clear=function() {
    this.main.innerHTML="";
}

Translations.prototype.translateFrom=function(current,startcat,lin_action) {
    this.current=current;
    this.startcat=startcat;
    this.lin_action=lin_action;
    this.get_translations();
}

Translations.prototype.get_translations=function() {
    with(this) {
	var c=current;
	var args={from:c.from,input:gf_unlex(c.input),cat:startcat}
	if(options.translate_limit) args.limit=options.translate_limit
	if(options.show_grouped_translations)
	    server.translategroup(args,bind(show_groupedtranslations,this));
	else
	    server.translate(args,bind(show_translations,this));
    }
}

Translations.prototype.target_lang=function() {
    with(this) return langpart(to_menu.value,grammar.name);
}

Translations.prototype.show_translations=function(translationResults) {
    var self=this;
    function tdt(tree_btn,s,action) {
	var txt=text(s);
	if(action) {
	    txt=node("span",{onclick:action},[txt])
	    //txt=button(s,action)
	}
	return self.options.show_trees ? tda([tree_btn,text(" "),txt]) : td(txt)
    }
    function act(lin) {
	return self.lin_action ? function() { self.lin_action(lin) } : null
    }
    with(self) {
	var trans=main;
	//var to=target_lang(); // wrong
	var to=to_menu.value;
	var cnt=translationResults.length; // cnt==1 usually
	//trans.translations=translations;
	trans.single_translation=[];
	trans.innerHTML="";
	/*
	  trans.appendChild(wrap("h3",text(cnt<1 ? "No translations?" :
	  cnt>1 ? ""+cnt+" translations:":
	  "One translation:")));
	*/
	for(var p=0;p<cnt;p++) {
	    var tra=translationResults[p];
	    var bra=tra.brackets;
	    if (tra.translations != null) {
		for (q = 0; q < tra.translations.length; q++) {
		    var t = tra.translations[q];
		    var lin=t.linearizations;
		    var tbody=empty("tbody");
		    if(options.show_abstract && t.tree) {
			function abs_act() {
			    self.options.abstract_action(t.tree)
			}
			var abs_hdr = options.abstract_action 
		                      ? title("Edit the syntax tree",
				              button("Abstract",abs_act))
			              : text("Abstract: ")
			tbody.appendChild(
			    tr([th(abs_hdr),
				tdt(node("span",{},[abstree_button(t.tree),
						    alignment_button(t.tree)]),
				    t.tree)]));
		    }
		    for(var i=0;i<lin.length;i++) {
			if(lin[i].to==to)
			    trans.single_translation.push(lin[i].text);
			if(lin[i].to==current.from && lin[i].brackets)
			    bra=lin[i].brackets;
			if(to=="All" || lin[i].to==to) {
			    var langcode=langpart(lin[i].to,grammar.name)
		          //var hdr=text(langcode+": ")
			    var hdr=title("Switch input language to "+langcode,
					  button(langcode,act(lin[i])))
			    //hdr.disabled=lin[i].to==current.from
			    var btn=parsetree_button(t.tree,lin[i].to)
			    tbody.appendChild(tr([th(hdr),
						  tdt(btn,lin[i].text)]));
			}
		    }
		    trans.appendChild(wrap("table",tbody));
		}
	    }
	    else if(tra.typeErrors) {
		    var errs=tra.typeErrors;
		    for(var i=0;i<errs.length;i++)
			trans.appendChild(wrap("pre",text(errs[i].msg)))
	    }
	    if(options.show_brackets)
		trans.appendChild(div_class("brackets",draw_brackets(bra)));

	}
    }
}

Translations.prototype.show_groupedtranslations=function(translationsResult) {
    with(this) {
	var trans=main;
	var to=target_lang();
	//var to=to_menu.value // wrong
	var cnt=translationsResult.length;
	//trans.translations=translationsResult;
	trans.single_translation=[];
	trans.innerHTML="";
	for(var p=0;p<cnt;p++) {
	    var t=translationsResult[p];
	    if(to=="All" || t.to==to) {
		var lin=t.linearizations;
		var tbody=empty("tbody");
		if(to=="All") tbody.appendChild(tr([th(text(t.to+":"))]));
		for(var i=0;i<lin.length;i++) {
		    if(to!="All") trans.single_translation[i]=lin[i].text;
		    tbody.appendChild(tr([td(text(lin[i].text))]));
		    if (lin.length > 1) tbody.appendChild(tr([td(text(lin[i].tree))]));
		}
		trans.appendChild(wrap("table",tbody));
	    }
	}
    }
}


Translations.prototype.abstree_button=function(abs) {
  var f=this.options.tree_img_format;
  var i=button_img(tree_icon,"toggle_img(this)");
  i.title="Click to display abstract syntax tree"
  i.other=this.server.current_grammar_url+"?command=abstrtree&format="+f+"&tree="+encodeURIComponent(abs);
  return i;
}

Translations.prototype.alignment_button=function(abs) {
  var f=this.options.tree_img_format;
  var i=button_img(alignment_icon,"toggle_img(this)");
  i.title="Click to display word alignment"
  i.other=this.server.current_grammar_url+"?command=alignment&format="+f+"&tree="+encodeURIComponent(abs);
  return i;
}

Translations.prototype.parsetree_button=function(abs,lang) {
  var f=this.options.tree_img_format;
  var i=button_img(tree_icon,"toggle_img(this)");
  i.title="Click to display parse tree"
  i.other=this.server.current_grammar_url
          +"?command=parsetree&format="+f+"&from="+lang+"&tree="+encodeURIComponent(abs);
  return i;
}

function draw_brackets(b) {
    return b.token
	? span_class("token",text(b.token))
	: node("span",{"class":"brackets",
		       title:(b.fun||"_")+":"+b.cat+" "+b.fid+":"+b.index},
	       b.children.map(draw_brackets))
}
