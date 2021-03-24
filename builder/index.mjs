import puppeteer from "puppeteer"
import fs from "fs"

async function main () {
  let graph = process.argv[2]
  let folder = fs.realpathSync(process.argv[3] || ".")

  let browser = await puppeteer.launch({
    headless: true,
  })

  let page = await browser.newPage()

  await page._client.send("Page.setDownloadBehavior", {
    behavior: "allow",
    downloadPath: folder,
  })

  await page.goto(`https://roamresearch.com/#/app/${graph}`)
  await page.waitForSelector(".bp3-icon-more")
  await page.waitForTimeout(5000)
  await page.waitForSelector(".bp3-icon-more")

  await page.click(".bp3-icon-more")
  await page.evaluate(function () {
    for (let x of document.querySelectorAll(".bp3-menu li a")) {
      if (x.innerText == "Export All")
        x.click()
    }
  })

  await page.waitForSelector(".bp3-dialog .bp3-button.bp3-minimal")
  await page.click(".bp3-dialog .bp3-button.bp3-minimal")
  await page.click(".bp3-dialog .bp3-overlay li:last-child")
  await page.click(".bp3-intent-primary")

  await page.waitForTimeout(3000)

  await browser.close()
}

main()
