(ns retrospect.problems.words.symbols)

(def punctuation-regex
  (re-pattern #"^([\，\。\、\.\?(\)\；\》\《\-\：\:\—\ \,\+\$\!\「\」\『\』\︰\？\（\）\／\·\·\．\“\”])$"))