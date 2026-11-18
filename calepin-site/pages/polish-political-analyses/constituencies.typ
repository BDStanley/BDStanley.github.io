#set document(title: [Constituencies])
#metadata((
  title: "Constituencies",
  description: "The Constituencies page is now the Geography page of the Polling tracker",
)) <website-metadata>

// The Constituencies page was removed on 18 November 2026 (Ben's decision; Election 2027/PLAN.md, decision 18, in the Polish
// political analyses repo): everything it did is now the Polling tracker's Geography address, polling-tracker/geography.typ.
// This page stays, out of the analyses index, so that old links land there. The script carries the old address's view, election,
// constituency and scenario across (#view=map|worth|seats|method, #y=, #c=, #sc=, #pa=); without scripts the meta refresh goes to
// Geography's first view.
#html.elem("script", "(function(){var q=new URLSearchParams(location.hash.slice(1)),o=new URLSearchParams(),v=q.get('view'),y=q.get('y');if(v==='worth')o.set('view','worth');else if(v==='seats')o.set('view','population');if(y&&y!=='2023')o.set('y',y);if((!v||v==='map'||v==='method')&&y&&y!=='2023')o.set('m','vvi_electors');['c','sc','pa'].forEach(function(k){if(q.get(k))o.set(k,q.get(k));});location.replace('polling-tracker/geography.html'+(o.toString()?'#'+o.toString():''));})();")
#html.elem("meta", attrs: (http-equiv: "refresh", content: "0; url=polling-tracker/geography.html"))

#heading(level: 1)[Constituencies]

The Constituencies page is now the #link("polling-tracker/geography.html")[Geography] page of the Polling tracker: what a vote is worth in each Sejm constituency since 1991, a profile of each constituency, and where the projected seats fall.
