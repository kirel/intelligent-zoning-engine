class StreetError < StandardError; end
class StreetNoError < StandardError; end
class ZeroSchoolsError < StandardError; end

def find_school(browser, street, no, zip)
  browser.visit "https://www.berlin.de/sen/jugend/familie-und-kinder/kindertagesbetreuung/kitas/umkreis/"

  browser.fill_in 'txtStrasse', with: street
  browser.fill_in 'txtHausNr', with: no
  browser.fill_in 'txtPLZ', with: zip

  browser.uncheck('cblEinzugsbereich$1')
  browser.check('cblEinzugsbereich$0')

  browser.click_on 'lbtnsuchen'

  browser.click_on 'lbtnsuchen' if browser.has_css?('#ddlStrassen')

  school_ids = browser.find_all('#DataListEinzugSchulen td a').map(&:text)

  raise ZeroSchoolsError if school_ids.length == 0

  return {
    area_text: browser.find('#lblEinzugSchulen').text,
    school_id: school_ids
  }
rescue
  if browser.has_text? "Die angegebene Strasse ist nicht bekannt"
    raise StreetError
  elsif browser.has_text? "Leider konnte keine Hausnummer gefunden werden"
    raise StreetNoError
  else
    raise
  end
end

# find_school 'Oppelner Straße', '33', '10997'
