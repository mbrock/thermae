api = roamAlphaAPI

function fix () {
  let nodes = api.q(`[
    :find (pull ?x [:node/title :block/uid])
    :where [?x :node/title]
  ]`)

   for (let [{ uid, title }] of nodes) {
     if (title.match(/^articles\/(.*)$/)) {
       title = `[[Article]] ${RegExp.$1}`
       console.log(title)
       api.updatePage({ page: { uid, title } })
     }
   }
}

function fix2 () {
  let nodes = api.q(`[
    :find (pull ?x [* {:block/refs [*]} {:block/page [*]}])
    :where [?x :block/refs ?ref]
      [?ref :node/title "Category"]
  ]`)

   for (let [x] of nodes) {
     if (["books", "articles"].includes(x.refs[1].title))
       api.deleteBlock({ block: { uid: x.uid }})
   }
}


fix2()
