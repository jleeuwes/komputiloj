function weblogFolderTree(e){var t=getById(e);if(t){for(var o,n,i=t.getElementsByTagName("ul"),a=0;o=i[a++];)n=o.parentNode,hasClass(n,"open")||(n.className="closed"),n.style.cursor="pointer";t.onclick=function(t){var o=t.target||t.srcElement;do{if("LI"==o.nodeName){o.parentNode.id==e&&toggleClass(o,"closed");break}}while(o=o.parentNode)}}}function WeblogReactionForm(e){this.reactionForm=getById("reactieForm"),this.reactionFormContent=null,this.editReactionId=e||0,this.init()}Object.extend(WeblogReactionForm.prototype,{init:function(){if(this.reactionForm&&window.RmlToolBar){this.reactionFormContent=this.reactionForm.elements.reactieFormContent,new RmlToolBar(this.reactionFormContent,"vertical","nl","weblogs");var e=getById("reacties");e&&(preload("reaction_quotecomment","g/if/comments/quotecomment.png"),Selector("p.author",e).forEach(this.addQuoteButton.bind(this)))}},addQuoteButton:function(e){var t,o,n=e.parentNode;hasClass(n,"reactie")&&n.id&&/^r_(\d+)$/.test(n.id)&&(t=n.id.substr(2))!=this.editReactionId&&((o=document.createElement("img")).className="quoteImg",o.src=getPreloadImage("reaction_quotecomment"),o.width=16,o.height=16,o.title="en"==document.documentElement.lang?"Quote comment":"Quote reactie",addEvent(o,"click",this.getQuote.bind(this,t)),e.insertBefore(o,e.firstChild))},getQuote:function(e){var t=Ajax.sendRequest(getXmlHttpUrl("weblogs","reaction","quote","id="+encodeURIComponent(e)),{method:"GET",type:"json",async:!1,nocache:!0,handler:checkJsonResponse});t&&(this.reactionFormContent.value+=t,this.reactionFormContent.focus())}});var SingleSignOn=function(){var e=function(e){e&&"data"in e&&Ajax.sendRequest(location.protocol+"//"+location.host+"/logincheck",{method:"POST",type:"json",async:!0,handler:t},objectToQueryString(e.data))},t=function(e){if(e&&"data"in e&&"success"===e.data){var t=getById("loginFrame");t&&(t.innerHTML='<h2>Ingelogd</h2><p>Je bent nu ingelogd met je Tweakers account; klik <a href="'+window.location.href.escapeHtml()+'" target="_top">hier</a> om de pagina te verversen.</p>')}};return{init:function(){Ajax.sendRequest(jsConfig.get("TnetBaseURL")+"ext/tweakblog_sso/",{method:"POST",type:"json",async:!0,withCredentials:!0,handler:e})}}}();