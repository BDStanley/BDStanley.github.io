#set document(title: [Legislative activity])
#metadata((
  title: "Legislative activity",
  description: "The Legislative activity page is now three pages: the Sejm, the Senate and the European Parliament",
)) <website-metadata>

// The Legislative activity page, which showed the Sejm, the Senate and the European Parliament with a body switch, was split
// on 21 November 2026 (Ben's request) into sejm.typ, senate.typ and european-parliament.typ, which embed the same app with
// ?b=sejm, ?b=senate and ?b=ep. This page stays, out of the analyses index, so that old links land on the right one: the
// script reads the old address's body (#b=senate or #b=ep; none meant the Sejm), carries the rest of the hash (#view=, #t=,
// #mp=, #q=, #topic=, #bill=, #rc=) and the query (?public) across, and replaces this page with that body's page. Without
// scripts the meta refresh goes to the Sejm.
#html.elem("script", "(function(){var q=new URLSearchParams(location.hash.slice(1)),b=q.get('b'),p=b==='senate'?'senate':b==='ep'?'european-parliament':'sejm';q.delete('b');var h=q.toString();location.replace(p+'.html'+location.search+(h?'#'+h:''));})();")
#html.elem("meta", attrs: (http-equiv: "refresh", content: "0; url=sejm.html"))

#heading(level: 1)[Legislative activity]

The Legislative activity page is now three pages: the #link("sejm.html")[Sejm], the #link("senate.html")[Senate] and the #link("european-parliament.html")[European Parliament].
