#import "/.calepin/calepin.typ" as calepin

#set document(title: [Media])
#metadata((title: "Media")) <website-metadata>

// No on-page title heading; nav label / tab title come from the metadata above.

= Kultura Liberalna

// The full archive lives in data/kultura-liberalna.yml and is refreshed by
// scripts/update-kl.py on every build (build.sh). It is NOT scraped from this
// page, for two reasons:
//   * Calepin caches chunk output against a fingerprint of the page source, so
//     the R scraper this replaced only re-ran when media.typ itself changed --
//     it had been serving a snapshot frozen on 21 June 2026.
//   * The author page only exposes the 15 most recent pieces (it is a single
//     `autor` post, not a paginated archive), so a pure scrape can never show
//     more than 15. The YAML accumulates instead, and only ever grows.
// To add a piece the scraper missed, or fix a title, edit the YAML by hand --
// the script keys on url and never overwrites an existing entry.
#let kl-date(iso) = {
  let p = iso.split("-")
  p.at(2) + "." + p.at(1) + "." + p.at(0)
}

#for a in yaml("/data/kultura-liberalna.yml").articles [
  - #link(a.url)[#a.title] (#kl-date(a.date))
]

= Polityka.pl

- #link("https://www.polityka.pl/tygodnikpolityka/kraj/2229078,1,co-pokazuja-najnowsze-sondaze-wladza-dla-pis-z-konfederacja-czy-dla-opozycji.read")[Co pokazują najnowsze sondaże? Władza dla PiS z Konfederacją czy dla opozycji?] (27.09.2023)
- #link("https://www.polityka.pl/tygodnikpolityka/kraj/2227338,1,sondaze-w-polowie-wrzesnia-pis-rosnie-konfederacja-slabnie-co-z-tego-wynika.read")[Sondaże w połowie września. PiS rośnie, Konfederacja słabnie. Co z tego wynika?] (14.09.2023)
- #link("https://www.polityka.pl/tygodnikpolityka/kraj/2225160,1,sierpniowe-sondaze-pis-troche-w-gore-ko-sie-trzyma-konfederacja-gasnie.read")[Sierpniowe sondaże. PiS trochę w górę, KO się trzyma, Konfederacja gaśnie] (28.08.2023)
- #link("https://www.polityka.pl/tygodnikpolityka/kraj/2220810,1,ko-moze-wyprzedzic-pis-w-sondazach-ale-czy-to-da-opozycji-wladze.read")[KO może wyprzedzić PiS w sondażach. Ale czy to da opozycji władzę?] (21.07.2023)
- #link("https://www.polityka.pl/tygodnikpolityka/kraj/2217564,1,co-pokazuja-sondaze-ko-blisko-pis-ale-niekoniecznie-blizej-wladzy.read")[Co pokazują sondaże? KO blisko PiS, ale niekoniecznie bliżej władzy] (27.06.2023)
- #link("https://www.polityka.pl/tygodnikpolityka/kraj/2216098,1,sondaze-po-4-czerwca-oboz-demokratyczny-na-razie-nie-zyskal.read")[Sondaże po 4 czerwca. Obóz demokratyczny (na razie) nie zyskał] (15.06.2023)
- #link("https://www.polityka.pl/tygodnikpolityka/kraj/2214326,1,sojusz-psl-i-holowni-w-sondazach-sukces-czy-porazka.read")[Sojusz PSL i Hołowni w sondażach. Sukces czy porażka?] (31.05.2023)
- #link("https://www.polityka.pl/tygodnikpolityka/kraj/2210842,1,wybory-do-sejmu-na-ostrzu-noza-sondaze-o-koalicji-psl-polska-2050.read")[Wybory do Sejmu na ostrzu noża. Sondaże o koalicji PSL-Polska 2050.] (28.04.2023)
- #link("https://www.polityka.pl/tygodnikpolityka/kraj/2208024,1,wnioski-z-ostatnich-sondazy-silniejsza-konfederacja-dla-kogo-to-jest-teraz-wyzwanie.read")[Wnioski z ostatnich sondaży. Silniejsza Konfederacja: dla kogo to jest teraz wyzwanie.] (05.04.2023)
- #link("https://www.polityka.pl/tygodnikpolityka/kraj/2203626,1,sondaze-po-lutym-umacnia-sie-dominacja-pis-i-koalicji-obywatelskiej.read")[Sondaże po lutym. Umacnia się dominacja PiS i Koalicji Obywatelskiej.] (01.03.2023)
- #link("https://www.polityka.pl/tygodnikpolityka/kraj/2199500,1,sondaze-jak-var-koalicja-obywatelska-coraz-blizej-pis.read")[Sondaże jak VAR. Koalicja Obywatelska coraz bliżej PiS.] (27.01.2023)
- #link("https://www.polityka.pl/tygodnikpolityka/kraj/2193686,1,kto-w-koncu-prowadzi-w-sondazach-odcinamy-szum.read")[Kto w końcu prowadzi w sondażach? Odcinamy szum.] (13.12.2022)
- #link("https://www.polityka.pl/tygodnikpolityka/kraj/2186994,1,ko-wyprzedza-pis-w-sondazach-jeszcze-nie-teraz-ale.read")[KO wyprzedza PiS w sondażach? Jeszcze nie teraz, ale…] (25.10.2022)
- #link("https://www.polityka.pl/tygodnikpolityka/kraj/2182024,1,pis-traci-aure-nietykalnosci-opozycja-w-sondazach-blizej-wladzy.read")[PiS traci aurę nietykalności. Opozycja w sondażach bliżej władzy.] (19.09.2022)
- #link("https://www.polityka.pl/tygodnikpolityka/kraj/2175811,1,przelom-w-partyjnych-sondazach-nie-tym-razem-trzy-powody.read?src=mt")[Przełom w partyjnych sondażach? Nie tym razem. Trzy powody.] (01.08.2022)
- #link("https://www.polityka.pl/tygodnikpolityka/kraj/2165629,1,co-slychac-w-sondazach-wraca-system-dwoch-i-pol-partii.read")[Co słychać w sondażach? Wraca system dwóch (i pół) partii.] (13.05.2022)
- #link("https://www.polityka.pl/tygodnikpolityka/kraj/2151110,1,sondaze-z-poczatku-roku-polska-2050-i-psl-nieco-w-gore-co-z-pis.read")[Sondaże z początku roku. Polska 2050 i PSL nieco w górę. Co z PiS?] (11.04.2022)
- #link("https://www.polityka.pl/tygodnikpolityka/kraj/2149542,1,rok-w-sondazach-wrocilismy-do-punktu-wyjscia-ale-patrzmy-na-trendy.read")[Rok w sondażach. Wróciliśmy do punktu wyjścia, ale patrzmy na trendy.] (01.01.2022)
- #link("https://www.polityka.pl/tygodnikpolityka/kraj/2144644,1,pis-i-ko-traca-w-sondazach-to-kto-zyskuje.read")[PiS i KO tracą w sondażach. To kto zyskuje?] (24.11.2021)
- #link("https://www.polityka.pl/tygodnikpolityka/kraj/2141692,1,duzy-spadek-pis-w-sondazu-cbos-to-przelom.read")[Duży spadek PiS w sondażu CBOS. To przełom?] (03.11.2021)
- #link("https://www.polityka.pl/tygodnikpolityka/kraj/2139238,1,mimo-grozby-polexitu-pis-wciaz-rosnie-w-sondazach.read")[Mimo groźby polexitu PiS wciąż rośnie w sondażach.] (15.10.2021)
- #link("https://www.polityka.pl/tygodnikpolityka/kraj/2134122,1,ko-silniejsza-w-sondazach-ale-opozycja-slabsza.read")[KO silniejsza w sondażach, ale opozycja słabsza.] (09.09.2021)
- #link("https://www.polityka.pl/tygodnikpolityka/kraj/2128493,1,wiecej-niz-efekt-tuska-ko-rosnie-w-sondazach-pis-buksuje-w-miejscu.read")[Więcej niż efekt Tuska. KO rośnie w sondażach, PiS buksuje w miejscu.] (02.08.2021)
- #link("https://www.polityka.pl/tygodnikpolityka/kraj/2126089,1,efekt-tuska-juz-widac-w-sondazach-koalicja-obywatelska-wraca-na-drugie-miejsce.read")[Efekt Tuska już widać w sondażach. Koalicja Obywatelska wraca na drugie miejsce.] (15.07.2021)
- #link("https://www.polityka.pl/tygodnikpolityka/kraj/2118683,1,lewica-w-sondazach-nie-zyskala-po-nie-przestala-tracic.read?src=mt")[Lewica w sondażach nie zyskała, PO nie przestała tracić.] (15.05.2021)
- #link("https://www.polityka.pl/tygodnikpolityka/kraj/2106033,1,wladza-kontra-opozycja-ko-kontra-holownia-co-mowia-sondaze.read")[Władza kontra opozycja, KO kontra Hołownia. Co mówią sondaże?] (23.02.2021)
- #link("https://www.polityka.pl/tygodnikpolityka/kraj/2101969,1,pis-bez-samodzielnej-wiekszosci-holownia-tuz-za-ko.read?src=mt")[PiS bez samodzielnej większości, Hołownia tuż za KO.] (02.02.2021)
- #link("https://www.polityka.pl/tygodnikpolityka/kraj/2101249,1,nie-tylko-aborcja-o-co-chodzi-zwolennikom-strajku-kobiet.read")[Nie tylko aborcja. O co chodzi zwolennikom Strajku Kobiet?] (30.01.2021)
- #link("https://www.polityka.pl/tygodnikpolityka/kraj/2097207,1,pis-odrabia-straty-w-sondazach-zyskuje-tez-holownia-i-lewica.read")[PiS odrabia straty w sondażach. Zyskuje też Hołownia i Lewica.] (31.12.2020)
- #link("https://www.polityka.pl/tygodnikpolityka/kraj/2074683,1,sondaz-w-sprawie-protestow-kto-bierze-udzial-w-spacerach.read")[Sondaż w sprawie protestów. Kto bierze udział w spacerach?] \[with Marta Żerkowska-Balas\] (30.12.2020)
- #link("https://www.polityka.pl/tygodnikpolityka/kraj/1980356,1,pis-wyraznie-traci-opozycja-moglaby-stworzyc-rzad.read")[PiS wyraźnie traci, opozycja mogłaby stworzyć rząd.] (26.11.2020)
- #link("https://www.polityka.pl/tygodnikpolityka/kraj/1978128,1,pis-wyraznie-stracil-w-sondazach-kto-na-tym-zyskal.read")[PiS wyraźnie stracił w sondażach. Kto na tym zyskał?] (06.11.2020)
- #link("https://www.polityka.pl/tygodnikpolityka/kraj/1968042,1,dobre-sondaze-ko-ruch-holowni-moze-jej-zaszkodzic.read")[Dobre sondaże KO. Ruch Hołowni może jej zaszkodzić.] (18.09.2020)
- #link("https://www.polityka.pl/tygodnikpolityka/kraj/1963436,1,ostatnie-sondaze-w-tej-kampanii-trzaskowski-o-wlos-przed-duda.read")[Ostatnie sondaże w tej kampanii. Trzaskowski o włos przed Dudą?] (10.07.2020)
- #link("https://www.polityka.pl/tygodnikpolityka/kraj/1962464,1,duda-minimalnie-przed-trzaskowskim-ale-do-rozstrzygniecia-daleko.read?src=mt")[Duda minimalnie przed Trzaskowskim, ale do rozstrzygnięcia daleko]. (03.07.2020)
- #link("https://www.polityka.pl/tygodnikpolityka/kraj/1961566,1,sondaze-duda-z-trzaskowskim-w-drugiej-turze-ile-ich-dzieli.read")[Sondaże: Duda z Trzaskowskim w drugiej turze. Ile ich dzieli?] (26.06.2020)
- #link("https://www.polityka.pl/tygodnikpolityka/kraj/1960778,1,stanley-dla-polityki-w-drugiej-turze-trzaskowski-juz-remisuje-z-duda.read?src=mt")[W drugiej turze Trzaskowski już remisuje z Dudą]. (19.06.2020)
- #link("https://www.polityka.pl/tygodnikpolityka/kraj/1959686,1,w-sondazach-drugiej-tury-z-duda-coraz-gorzej.read?src=mt")[W sondażach drugiej tury z Dudą coraz gorzej]. (09.06.2020)
- #link("https://www.polityka.pl/tygodnikpolityka/kraj/1957640,1,trzaskowski-wchodzi-do-gry-co-widac-w-sondazach.read")[Trzaskowski wchodzi do gry. Co widać w sondażach?] (22.05.2020)
- #link("https://www.polityka.pl/tygodnikpolityka/kraj/1954530,1,co-widac-w-sondazach-duda-wygrywa-przy-niskiej-frekwencji.read")[Co widać w sondażach? Duda wygrywa przy niskiej frekwencji.] (29.04.2020)
- #link("https://www.polityka.pl/tygodnikpolityka/kraj/1953522,1,rzad-dezinformuje-o-unii-czy-polacy-mu-wierza.read")[Rząd dezinformuje o Unii. Czy Polacy mu wierzą?] (22.04.2020)
- #link("https://www.polityka.pl/tygodnikpolityka/kraj/1949534,1,stanley-dla-polityki-duda-wciaz-faworytem-ale-drugiej-tury.read")[Stanley dla „Polityki”: Duda wciąż faworytem, ale drugiej tury]. (25.03.2020)

= Other

- #link("https://www.newsweek.pl/polska/polityka/pis-zmienilo-polske-glebiej-niz-sie-zdaje-donald-tusk-przejal-ich-jezyk/2lp9v76")[Polityczne szczęście PiS się wyczerpało? "Sławomir Mentzen wszedł do mateczników"] \[Newsweek, interviewed\] (17.02.2026)
- #link("https://newbooksnetwork.com/democracy-after-illiberalism-a-warning-from-poland")[Democracy After Illiberalism: A Warning from Poland] \[_New Books Network_, podcast\] (28.10.2025)
- #link("https://www.youtube.com/watch?v=TI4BULBuqBs")[Democracy After Illiberalism] \[_Converging Dialogues_, podcast\] (09.10.2025)
- #link("https://revdem.ceu.edu/2025/09/01/illiberal-trap/")[Illiberal Trap] \[_Journal of Democracy in Central and Eastern Europe_, podcast\] (01.09.2025)
- #link("https://europe.columbia.edu/content/rise-and-resilience-populism-eastern-europe")[The Ambivalence of Polish Populism] \[_Rise and Resilience of Populism in Eastern Europe, Columbia European Institute_, interview\] (20.01.2022)
- #link("https://www.washingtonpost.com/world/2021/12/27/poland-media-law-duda/")[Polish president vetoes controversial media law decried by U.S.] \[_The Washington Post_, interview\] (27.12.2021)
- #link("https://www.theguardian.com/world/2020/nov/06/a-backlash-against-a-patriarchal-culture-how-polish-protests-go-beyond-abortion-rights")[‘A backlash against a patriarchal culture’: How Polish protests go beyond abortion rights.] \[_The Guardian_, interview\] (06.11.2020)
- #link("https://oglobo.globo.com/mundo/erosao-continua-da-democracia-na-hungria-na-polonia-provoca-impasse-na-uniao-europeia-1-24709374")[Erosão contínua da democracia na Hungria e na Polônia provoca impasse na União Europeia.] \[_O Globo_, interview\] (24.10.2020)
- #link("https://www.ft.com/content/d504c841-d1e5-4a7d-9d7c-e2a2ed9f6fe1")[What’s at stake as Kaczynski returns to Poland’s front line?] \[_Financial Times_, interview\] (01.10.2020)
- #link("https://www.ft.com/content/640bc895-8232-4225-9a95-363a779ff531")[Law & Justice loses touch with Poland’s moderate Catholics.] \[_Financial Times_, interview\] (28.09.2020)
- #link("https://www.ft.com/content/570e24f9-057c-4a39-b81c-7f90e1eb6f85")[Poland heads for presidential run-off.] \[_Financial Times_, interview\] (29.06.2020)
- #link("https://www.newsweek.pl/ben-stanley-brytyjski-politolog-o-dumie-z-polski-rozmowa/8jtd8t6")[Czego brakuje Polakom? Odpowiada brytyjski politolog mieszkający w Polsce.] \[Newsweek, interview\] (16.12.2018)
