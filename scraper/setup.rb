# Require the gems
require 'capybara/poltergeist'

# Configure Poltergeist to not blow up on websites with js errors aka every website with js
# See more options at https://github.com/teampoltergeist/poltergeist#customization
Capybara.register_driver :poltergeist do |app|
  Capybara::Poltergeist::Driver.new(
    app,
    js_errors: false,
    logger: nil,
    phantomjs_logger: Logger.new('/dev/null'),
    phantomjs_options:
      [
        '--load-images=no',
        '--ignore-ssl-errors=yes'
      ]
  )
end

# Configure Capybara to use Poltergeist as the driver
Capybara.default_driver = :poltergeist
