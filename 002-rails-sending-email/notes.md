# How to Create, Preview, and Send Email From Your Rails App

## Intro

- Today in the Studio I'll show you how to create, preview, and send emails from your Rails app

- Hey folks, Mike Clark here with the Pragmatic Studio.

- Recently I upgraded a Rails app and was reminded of some of the nifty new features in ActionMailer

- So today I'll walk you step-by-step through my workflow for sending emails from a Rails app

- Let's jump right into it...

## The Application

- Our application lets users list items for sale

- And other users can leave comments about an item

- When a comment is posted, we want to send an email notification to the user who listed that item for sale

- Let's take it step by step...

## Configuration

- First we need to do a wee bit of configuration, so let's just get that out of the way

- We'll be sending emails from the `development` environment, so I'm in the `config/environments/development.rb` file...

- I'll show you how to set up for production a bit later

- First, change `raise_delivery_errors` to `true`:

    ```ruby
    config.action_mailer.raise_delivery_errors = true
    ```

- By default Rails will silently ignore any errors related to sending email, which isn't very helpful

- Setting this to `true` raises an exception if there's trouble sending an email

- Next, we need to arrange things so that our emails are delivered out into the world by an SMTP server 

- We'll use Gmail since most folks already have a Google account:

- I'll just paste in the configuration at the bottom:

    ```ruby
    config.action_mailer.delivery_method = :smtp

    config.action_mailer.smtp_settings = {
      address: "smtp.gmail.com",
      port: 587,
      domain: "example.com",
      authentication: "plain",
      enable_starttls_auto: true,
      user_name: ENV["GMAIL_USERNAME"],
      password: ENV["GMAIL_PASSWORD"]
    }

    config.action_mailer.default_url_options = { host: "localhost:3000" }
    ```

- Don't sweat the details here. The important parts are...
    + `smtp` is the protocol for delivering mail messages
    + `address` and `port` are address of remote mail server
    + `domain` is name of your domain
    + `user_name` and `password` to authenticate with the mail server

- It's best practice to access credentials such as username and password via environment variables, as we've done here. 

- Accessing `ENV["GMAIL_USERNAME"]` returns the value of that environment variable.

- Otherwise, if you hard-code your secret credentials in this file then they're no longer a secret. Anyone who has access to your code (on a public GitHub repo for example) can plainly see your username and password.

- Environment variables also work great for video training!

- You never want to expose sensitive information in source code!

- Finally, we need to set the `:host` option which is used to properly generate absolute URLs within our email.

    ```ruby
    config.action_mailer.default_url_options = { host: "localhost:3000" }
    ```

- MUST RESTART SERVER for these changes to get picked up!
    
## Generate the Mailer

- First, we need to create a new mailer to render and send the email...

- Rails includes a generator to make it easy to create mailers:

    ```sh
    rails g mailer
    ```

- Pass it the name of the mailer and (optionally) a list of names for the
  emails we want to send

- We'll call our mailer `CommentMailer` and the email `new_comment`:

    ```sh
    rails g mailer CommentMailer new_comment
    ```

- Generates all the files we need, which we'll look at in turn...

## Review Generated Mailer

- Let's start with what got generated in the `app/mailers` directory

- Generated a base class `ApplicationMailer` that all mailer classes inherit from:

    ```ruby
    class ApplicationMailer < ActionMailer::Base
      default from: "from@example.com"
      layout 'mailer'
    end
    ```

- `default` is a hash of default values for any email you send from subclasses

- Change the default `from` to the email address you want use as the sender's
  address (address it's being sent from):

    ```ruby
    default from: "support@example.com"
    ```

- Then it has the name of the layout which we'll return to later:

    ```ruby
    layout 'mailer'
    ```

- We'll return to `layout` a bit later...

- Also generated a `CommentMailer` subclass that inherits from
  `ApplicationMailer`:
 
    ```ruby
    class CommentMailer < ApplicationMailer
      def new_comment
        @greeting = "Hi"

        mail to: "to@example.org"
      end
    end
    ```

- Mailers work similarly to controllers:
    - inherit from a common base class
    - methods similar to controller actions
    - instead of generating an HTML response, mailer methods generate an email
    - `new_comment` method corresponds to the name of our email (one method per email)
    - instance variables defined in the method are accessible in view templates
    
- Last line must call `mail` method to create a mail message and return it
    - can pass options via a hash
  
- `mail` method renders the email templates
  
## Review Generated Templates

- Generator created two view templates to render the mail message in the `app/views/comment_mailer` directory

- File names correspond to mailer method:
    + template for generating an HTML email is in `new_comment.html.erb`
    + template for generating a plain-text email is in `new_comment.text.erb`

- Just like a controller's view templates, mailer templates are a mix of static and dynamic content generated using ERb 

- `@greeting` is defined in the `new_comment` method, so it's accessible here

- It's a good practice to send both plain-text and HTML emails and let the email client decide which one to display

- And Action Mailer does all the heavy-lifting for us!

- Because we have two view templates here with the same name but different content types embedded in the filename, both templates will get rendered and sent out in one multi-part email

## Update the Mailer Templates

- Since we're already here, let's change the templates to generate the email content we want...

- I generally start by defining what the email will look like, and then backfill to set everything up

- Here's the plain-text version:

    ```erb
    A new comment was posted for your <%= @item.name %>:

      <%= @comment.body %>

    To reply to this comment, please visit the item page at

      <%= item_url(@item) %>
    ```

- To generate the URL, we use a named route helper method

- Important to use `_url` which generates an absolute URL rather than `_path` which generates relative URLs. Email client needs full URL.

- Can use helper methods just like any Rails view 

- Here's the HTML version (copy/paste) which renders the same content, but in structured HTML:

    ```erb
    <p>
      A new comment was posted for your <%= link_to @item.name, @item %>:
    </p>
    <blockquote>
      <em><%= @comment.body %></em>
    </blockquote>
    <p>
      To reply to this comment, please visit the
      <%= link_to "item page", @item %>.
    </p>
    ```

- We're in a template, so helper methods such as `link_to` are available

- Both these templates require `@comment` and `@item` instance variables defined, so that's our next step...

## Update the Mailer

- Back in `CommentsMailer`, the `new_comment` method needs to define two instance variables:
  - `@comment` is the new comment that was posted
  - `@item` is the item associated with that comment
  
    ```ruby
    def new_comment
      @comment
      @item = @comment.item

      mail to: "to@example.org"
    end
    ```

- Where does the comment come from?

- Unlike a controller action, a mailer method doesn't have access to request parameters

- So we need to pass in the comment as an argument:

    ```ruby
    def new_comment(comment)
      @comment = comment
      @item = @comment.item

      mail to: "to@example.org"
    end
    ```

- User who listed the item for sale should receive the email

- Change `to` to use that user's email address:

    ```ruby
    def new_comment(comment)
      @comment = comment
      @item = comment.item

      mail to: @item.user.email
    end
    ```

- We can also specify the `subject` of the email...

- If you want to support multiple languages, you can set the subject in the internationalization file (see comment)

- But we'll just set it directly:

    ```sh
    mail to: @item.user.email, subject: "New Comment for #{@item.name}"
    ```

- The `mail` method takes a number of options and I'll link to the documentation in the notes 

- Remember that the base class calls `default` method to set a default `from`address and any defaults set there get applied to the `mail` call

- This can be overridden on a per-email basis!

## Preview

- Our mailer and templates are good to go

- But before actually sending emails, I like to preview the rendered plain-text and HTML emails to make sure they look right

- In the old days, you know, like a couple years ago, you had to install a gem to preview emails

- But nowadays email previewing is built in!

- Preview file was generated in `test/mailers/previews` directory

    ```ruby
    def new_comment
      CommentMailer.new_comment
    end
    ```

- Notice it's calling `new_comment` as a class method, but it's defined as an instance method

- Don't let this throw you: Rails uses a sleight of hand here - instantiating a new `CommentMailer` object and calling the `new_comment` instance method

- Remember, we need to pass a comment to that method:

    ```ruby
    def new_comment
      comment = Comment.last
      CommentMailer.new_comment(comment)
    end
    ```

- Important to note that although preview files live under the `test` directory
  by default, when you preview an email it's running inside of the
  `development` environment

- So we can use development data - `Comment.last` queries the development
  database

- And the `new_comment` method creates and returns a mail message

- We can preview it in the browser: <http://localhost:3000/rails/mailers/comment_mailer>

- Check HTML and plain-text versions

## Layouts

- Now that I see these, I realize it would be nice to add a standard signature to the bottom...

- We could add text to the bottom of both the plain-text and HTML templates, but there's a better way...

- Just like controller views, mailer views have a layout file

- Default layout is set back in the `ApplicationMailer`:

    ```ruby
    class ApplicationMailer < ActionMailer::Base
      default from: "no-reply@example.com"
      layout 'mailer'
    end
    ```

- Since it's defined here, it applies to all mailers (subclasses) but can be overridden

- Layouts are in `app/views/layouts` corresponding to plain-text and HTML formats:
    + `mailer.html.erb`
    + `mailer.text.erb`

- Just as with controller views, calling `yield` renders the view (the mail content) inside the layout

- Update `app/views/layouts/mailer.html.erb`:

    ```erb
    <%= yield %>
    <p>
      Thanks!
    </p>
    ```
 
- Update `app/views/layouts/mailer.text.erb`:

    ```erb
    <%= yield %>

    Thanks!
    ```

- Preview again...

- Now any emails we generate will have a consistent signature!

- I love that we can now preview emails using the same rapid feedback loop as any other view template

## Send Email from the Console

- Everything looks good, so now we're ready to actually send a test email...

- To make sure everything is configured properly, I like to fire one email off from the console

- Same two lines of code as we used in the preview file:

    ```ruby
    >> comment = Comment.last
    >> mail = CommentMailer.new_comment(comment)
    ```

- Again, this feels odd that we're calling it as a **class method**, but just go with it...

- Notice the `new_comment` method creates and returns a `Mail::Message` object but DOES NOT SEND IT!

- Notice it generates one _multipart_ email:

    ```sh
    Content-Type: multipart/alternative;
    ```

- Tells the email client that the email contains multiple "alternative" representations of the same email body, and the client can choose to display whichever format it prefers

- ActionMailer knew to include two parts because we have two email templates (plain-text and HTML), so when the `new_comment` method was called it rendered both templates and created a `multipart/alternative` email.

- That's a good example of the power of conventions!

- To deliver the email immediately:

    ```ruby
    >> mail.deliver_now
    ```

- Again, but in one step:

    ```ruby
    >> comment = Comment.first
    >> CommentMailer.new_comment(comment).deliver_now
    ```

- This is a synchronous call: have to wait until the email is sent

- Alternatively, calling `deliver_later` queues up the email to be sent in the background using ActiveJob instead of delivering it immediately

- But we don't have a queuing system configured so we'll save that for another time. If there's interest, I may (ahem) queue that up for a future Studio session!

- Check Gmail account and should have TWO emails!

- COPY this line from console before moving on:

    ```ruby
    CommentMailer.new_comment(comment).deliver_now
    ```

## Integrate Into Controller

- With the mailer complete, the final step is to hook it into a our app

- We want to send the email whenever a comment is posted, and that happens over in the `create` action of the `CommentsController`:

- Paste line we used in console, then change to `@comment`:

    ```ruby
    def create
      @item = Item.find(params[:item_id])
      @comment = @item.comments.new(comment_params)
      @comment.user = current_user
      @comment.save!

      CommentMailer.new_comment(@comment).deliver_now

      ...
    end
    ```

- Try it out by posting a new comment: "Where are the handlebars?"

- Check Gmail account!

## Production Settings

- We have everything configured to send emails when running in the `development` environment, but what about when you go into production?

- Gmail limits the number of emails you can send, so it's good for low-volume transactional emails 

- But in production I recommend using another SMTP relay service such as Mailgun, Sendgrid, or Mandrill. 

- They can handle larger volumes, keep your emails from being filtered as spam, and can even track deliveries. Most have free plans that allow a reasonable number of emails per month. 

- I'll include links to these services in the notes.

- Supposing we picked one of these services, here are the changes we'd make...

- Copy settings from `development.rb` as a starting point

- Production settings go in the `production.rb` file so paste it 

- You'll need to change `address`, `user`, and `password`

- Important! Also need to change `:host` so that mailers generate the proper absolute URLs back to your production server:

    ```ruby
    config.action_mailer.default_url_options = { host: "pragmaticstudio.com" }
    ```

## Outro

- That wraps up this session!

- Hopefully it helps you get started sending emails or upgrade your app to take advantage of the new features

- As always, if you found this helpful please leave a comment below

- See ya next time!

## Show Notes

- [mail method documentation](http://api.rubyonrails.org/classes/ActionMailer/Base.html#method-i-mail)

For transactional email, consider using a service such as:

- [Mailgun](http://www.mailgun.com/)
- [Mandrill by MailChimp](http://mandrill.com/)
- [SendGrid](http://sendgrid.com)
- [Amazon Simple Email Service](http://aws.amazon.com/ses/pricing/)

For newsletters, announcements, and **bulk** email, consider using a service such as:

- [MailChimp](http://mailchimp.com/) 
- [Campaign Monitor](http://www.campaignmonitor.com)
- [Mad Mimi](https://madmimi.com/)
