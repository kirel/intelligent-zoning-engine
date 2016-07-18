require 'csv'
require './scraper/setup'
require './scraper/scrape'
require "retries"
require "work_queue"
require "innertube"
require "progress_bar"

class ProgressBar
  def write
    print "#{to_s}\n"
  end
end

noscrape = Set.new
done = Set.new

fresh = !File.exists?('data/umkreissuche.csv')

BROWSERS = 12
browser_pool = Innertube::Pool.new(proc { Capybara::Session.new(:poltergeist) }, proc { |browser| browser.driver.reset! })

pool = WorkQueue.new BROWSERS, 20
mtx = Mutex.new
etx = Mutex.new

if not fresh
  CSV.foreach("data/umkreissuche.csv", headers: :first_row) do |row|
    done << [row['STN'], row['HNR'], row['PLZ']].join('_')
  end
end

if File.exists?('data/umkreissuche_error.csv')
  # just contains concatenated strings
  CSV.foreach("data/umkreissuche_error.csv") do |row|
    noscrape << row[0]
  end
end

CSV.open("paerror.csv", "ab") do |error_csv|
CSV.open("paresult.csv", "ab") do |csv|
  csv << ["STN", "HNR", "PLZ", "school_id", "area_text"] if fresh
  $stderr.print "Starting parallel processing...\n"
  source_csv = CSV.foreach("data/HKO_2015_EPSG5650.txt", encoding: 'ISO-8859-1', col_sep: ';', headers: :first_row)
  progress = ProgressBar.new(source_csv.count, :counter, :percentage, :eta)
  source_csv.each do |row|
    street, no, zip = [row['STN'].encode("UTF-8"), row['HNR'].encode("UTF-8"), row['PLZ'].encode("UTF-8")]
    if noscrape.include? street
      $stderr.print "skipping #{street} (no scrape list)\n"
      progress.max -= 1
      next
    end
    if noscrape.include? [street, no, zip].join('_')
      $stderr.print "skipping #{street} #{no} #{zip} (no scrape list)\n"
      progress.max -= 1
      next
    end
    if done.include? [street, no, zip].join('_')
      $stderr.print "skipping #{street} #{no} #{zip} (already done)\n"
      progress.max -= 1
      next
    end
    $stderr.print "queueing #{street} #{no} #{zip}\n"
    done << [street, no, zip].join('_')
    pool.enqueue_b do
      begin
        with_retries(:max_tries => 5) do
          browser_pool.take do |browser|
            begin
              scrape_result = find_school(browser, street, no, zip)
              school_ids, area_text = scrape_result.values_at(:school_id, :area_text)
              school_ids.each do |school_id|
                mtx.synchronize { csv << [street, no, zip, school_id, area_text] }
              end
              $stderr.print "success #{street} #{no} #{zip}\nfound #{school_ids.length} schools (#{school_ids.join(', ')})\n"
            rescue StreetError
              $stderr.print "#{street} failed (StreetError)\n"
              $stderr.print "adding #{street} to no scrape list\n"
              noscrape << street
              etx.synchronize { error_csv << [street, 'StreetError'] }
            rescue StreetNoError
              $stderr.print "#{street} #{no} #{zip} failed (StreetNoError)\n"
              noscrape << [street, no, zip].join('_')
              etx.synchronize { error_csv << [[street, no, zip].join('_'), 'StreetNoError'] }
            rescue ZeroSchoolsError
              $stderr.print "#{street} #{no} #{zip} failed (ZeroSchoolsError)\n"
              noscrape << [street, no, zip].join('_')
              etx.synchronize { error_csv << [[street, no, zip].join('_'), 'ZeroSchoolsError'] }
            rescue Capybara::Poltergeist::TimeoutError
              $stderr.print "ZOMG timeout retry\n"
              browser.driver.restart
              raise
            end
          end # /browser_pool
        end # /retries
      rescue => e
        browser.save_screenshot("screens/#{[street, no, zip].join('_')}.png")
        puts "#{street} #{no} #{zip} permanently failed with #{e}"
      ensure
        progress.increment!
      end
    end # pool
  end # CSV.foreach
  pool.join
end # CSV.open
end # CSV.open
$stderr.print "done.\n"
