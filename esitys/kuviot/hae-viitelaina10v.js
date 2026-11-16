// Hakee Suomen valtion 10 vuoden viitelainan koron kuukausikeskiarvot Suomen
// Pankin raporttipalvelusta ja kirjoittaa ne csv-tiedostoon.
//
// Suomen Pankin korkorajapinnassa (/api/interestrates/) ei ole viitelainoja,
// ja raportin oletusnakyma kattaa vain kuluvan vuoden. Pidempi jakso saadaan
// vain raporttisivun lomakkeella, joten sivu avataan selaimella ja taulukko
// luetaan sivulta.
//
// Ajo: node esitys/kuviot/hae-viitelaina10v.js [tiedosto.csv] [alkuvuosi]

const { chromium } = require('playwright');
const fs = require('fs');
const out = process.argv[2] || 'esitys/kuviot/viitelaina10v.csv';
const alkuvuosi = process.argv[3] || '2015';
const KUUT = ['tammikuu', 'helmikuu', 'maaliskuu', 'huhtikuu', 'toukokuu', 'kesäkuu',
              'heinäkuu', 'elokuu', 'syyskuu', 'lokakuu', 'marraskuu', 'joulukuu'];
(async () => {
  const b = await chromium.launch();
  const p = await b.newPage();
  await p.goto('https://reports.suomenpankki.fi/WebForms/ReportViewerPage.aspx?report=/tilastot/arvopaperimarkkinat/velkapaperit/viitelainojen_korot_v2_fi&output=', {waitUntil: 'networkidle', timeout: 90000});
  const set = async (n, v) => {
    await p.selectOption('select[name="ReportViewer1$ctl04$' + n + '$ddValue"]', v);
    await p.waitForLoadState('networkidle');
    await p.waitForTimeout(1500);
  };
  await set('ctl05', '1');   // Aggregointi: keskiarvo
  await set('ctl11', '3');   // kuukausitasolla
  // Alkupaiva: valikon vanhin paiva halutulta vuodelta.
  const alku = await p.evaluate((v) => {
    const s = document.querySelectorAll('select')[2];
    const o = [...s.options].filter(o => o.text.endsWith('.' + v)).pop();
    return o && o.value;
  }, alkuvuosi);
  if (!alku) throw new Error('Alkuvuotta ' + alkuvuosi + ' ei ole valikossa.');
  await set('ctl07', alku);
  await p.click('#ReportViewer1_ctl04_ctl00');
  await p.waitForLoadState('networkidle', {timeout: 120000});
  await p.waitForTimeout(8000);
  const rivit = await p.evaluate(() =>
    [...document.querySelectorAll('#ReportViewer1_ctl09 tr')]
      .filter(r => r.querySelectorAll('tr').length === 0)
      .map(r => [...r.cells].map(c => c.innerText.trim())));
  const otsikko = rivit.find(r => r.includes('Ajanjakso'));
  if (!otsikko) throw new Error('Raporttitaulukkoa ei loytynyt sivulta.');
  const iAika = otsikko.indexOf('Ajanjakso');
  const iArvo = otsikko.indexOf('Valtion obligaatioiden korko, 10 vuotta');
  const csv = [];
  for (const r of rivit) {
    const m = /^(\S+) (\d{4})$/.exec(r[iAika] || '');
    const kk = m ? KUUT.indexOf(m[1].toLowerCase()) : -1;
    const arvo = parseFloat((r[iArvo] || '').replace(',', '.'));
    if (kk < 0 || Number.isNaN(arvo)) continue;
    csv.push(m[2] + '-' + String(kk + 1).padStart(2, '0') + '-01,' + arvo);
  }
  csv.sort();
  fs.writeFileSync(out, '"time","values"\n' + csv.join('\n') + '\n');
  console.log('Tallennettu:', out, '(' + csv.length + ' kuukautta)');
  await b.close();
})();
