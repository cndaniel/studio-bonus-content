# How To Add jQuery and Ajax To Your Rails App

## Intro

- Today in the Studio I'll show you how to add a dash of JavaScript to your Rails app

- Hey folks, Mike Clark here with the Pragmatic Studio.

- Today we're going to spice up a Rails app with a couple dashes of JavaScript: we'll add some effects with jQuery and send Ajax requests.

- If you're comfortable with Rails but new to JavaScript, this will wet your appetite for what's possible.

- And if you're already familiar with JavaScript, this will show you how to start using it in Rails

- It's gonna be a lot of fun, so let's get started!

## The Application

- Our application lets users list items for sale

- Now that spring is finally here, it's time to clean out the garage

- For example, here's a unicycle...

- Three folks left a comment

- And we have a form for writing a new comment

- But we don't want to display the comments and form by default

- Instead, we want to click the "Comments" link to toggle visibility

- We can do that dynamically with JS

## Show Template

- Let's look at the structure of the `items/show.html.erb` template

- Comment stuff is in a section with the ID `comments-section`:

    ```erb
    <section id="comments-section">
    ```

- Then an ordered lists with the ID `comments`:

    ```erb
    <ol id="comments">
      <%= render @comments %>
    </ol>
    ```

- We render the collection of @comments, which by convention calls the 
`comments/_comment.html.erb` partial in a loop - once for each comment

- Here's that partial:

    ```erb
    <li>
      <%= link_to comment.user.name, comment.user %>
      ...
    </li>
    ```

- The local variable `comment` refers to the comment being rendered 

- Then we display a form for posting a new comment

- Form POSTS to `create` action of `CommentsController`

- Helps to start with a good structure!

## Hide Comments Section

- First we'll hide the comment area

- We can do that with CSS

- Add a rule to `comments.scss`:

    ```css
    #comments-section {
      display: none;
    }
    ```

- The CSS selector is `#comments-section`

- Reload and it's gone!

- Next we need to show the comments section when the link is clicked

## Create JS File

- By default, Rails uses a technique called "Unobtrusive JavaScript" to attach JavaScript to elements in the DOM

- To make that work, we need a way to identify the link in the DOM:

    ```erb
    <%= link_to "Comments", "#", id: "comments-link" %>
    ```

- Now it's time to write some JavaScript!

- Show that `application.js` is a manifest file that requires everything in the directory by default:

    ```sh
    //= require_tree .
    ```

- Any files in this directory will be packaged up in a single file (minified and compressed in production) which means that it'll get downloaded on the first page load and then be cached on every page after that.

- Create `comments.coffee` to `app/assets/javascripts` directory

- If you want to use JavaScript, just create a `.js` file in the directory

- By default, a new Rails app is configured to use CoffeeScript which compiles down to JavaScript

## Wait for Document to Be Ready

- First we need to make sure the document is fully loaded before running any JS

    ```javascript
    $()
    ```

- `$` is a shortcut for the `jQuery` function which you'll use a lot
  - it's like Swiss Army knife for hacking through the DOM
  - it always returns a jQuery object

- We want the whole HTML document as a jQuery object:

    ```javascript
    $(document)
    ```

- Call the `on` method with TWO PARAMETERS - event and function to handle event:

    ```javascript
    $(document).on "page:change", ->
    ```

- In this case, we're listening for a `page:change` event

- `->` is a function to run when the event is fired - document is fully loaded

- Rails plays some tricks to speed up page rendering when links are clicked (Turbolinks) so this is the recommended way to wait for the page to be ready

## Basic Link Click

- When the page is ready, we need to attach an event handler to be run when the element with the ID `comments-link` is clicked

- First we need to find (select) the element with the ID `comments-link`

- The `$` method accepts a CSS selector as a string:

    ```javascript
    $(document).on "page:change", ->
      $('#comments-link')
    ```

- That finds the HTML element in the DOM and returns a jQuery object representing that element

- Then call the `click` method on that object and attach a function to run when the click event is fired:

    ```javascript
    $(document).on "page:change", ->
      $('#comments-link').click ->
        alert "Clicked!"
    ```

- Whitespace matters! 2-space indenting

- Great - everything is hooked up correctly!

## Toggle Comment Section

- Now we want to toggle the comments section...

- First we need to find (select) the element with the ID `comments-section`:

    ```javascript
    $('#comments-section')
    ```

- Then call the `toggle` method on the object:

    ```javascript
    $('#comments-section').toggle()
    ```

- `toggle()` simply toggles the visibility of that element by changing the CSS `display` property

- Toggle it back and forth

- If we want to get fancy, we can fade it:

    ```javascript
    $('#comments-section').fadeToggle()
    ```

## Autofocus

- It's also polite to put the cursor in the comment box so you're ready to type

- By convention, Rails assigns an ID to the text area based on the model name and attribute name:

    ```javascript
    $('#comment_body').focus()
    ```

- Then call `focus`

- Refresh and cursor should be in comment box

## Show Typical Form Handling

- Post the comment "Where are the handlebars?"

- Full request/response cycle:
	+ When form is submitted, browser sends a POST request to the server
	+ New comment is created
	+ Redirects to the item, which tells the browser to issue a new GET request for the item (`show` page)
	+ Server sends an HTML response back to the browser
	+ Entire page is loaded

- But posting a comment and having to wait for a full page reload just to see our new comment is the long way around the barn

## Asynchronous Form Handling

- Instead, we want to do this asynchronously behind the scenes often referred to as an Ajax request:
	+ When form is submitted, JS asynchronously sends a POST request to the server
	+ New comment is created
	+ Server generates a JS response that append the HTML for the new comment to the comment list
	+ Browser evaluates the JavaScript returned by the server, which then modifies the DOM
	+ So we're updating the DOM dynamically rather than reloading the entire page

## Update Form

- Back in `items/show.html.erb`, the `form_for` helper takes a `remote` option:

    ```erb
    <%= form_for [@item, @item.comments.new], remote: true do |f| %>
    ```

- The `remote: true` options tells Rails to send the POST request asynchronously (Ajaxified form) and expect a JS response

- This option is also available on `link_to` and `button_to`

## Respond with JS

- Now that we have an Ajaxified form, the next step is to implement the functionality on the server to respond to the request appropriately.

- Form POSTS to the `create` action of the `ItemsController`:

    ```ruby
    def create
      @item = Item.find(params[:item_id])
      @comment = @item.comments.new(comment_params)
      @comment.user = current_user
      @comment.save!

      redirect_to @item
    end
    ```

- Currently, it responds by redirecting

- To respond to an Ajax (JS) request, add a `respond_to` block:

    ```ruby
    respond_to do |format|
      format.html { redirect_to @item }
    end
    ```

- `respond_to` lets us to respond appropriately depending on what the "client" wants

- `format.html` responds to the regular HTML requests (redirecting in this case)

- `format.js` responds to JS requests:

    ```ruby
    format.js # renders comments/create.js.erb
    ```

- By convention, it looks for `comments/create.js.erb` to render the response

## Generate JS Response

- Create `comments/create.js.erb` template that renders (generates) the JavaScript code which gets sent back to the browser

- We need to generated JS code that appends some HTML to the end of the element with the ID `comments` (our comment list):

    ```javascript
    $("#comments").append("...html...");
    ```

- (This isn't CoffeeScript, so we need to use semicolons!)

- Finds the HTML element with ID `comments` (that's our list) 
- Then we can use `append` to append some HTML
- What HTML?
- Well, we want to append the snippet of HTML that shows the new comment (`@comment`)

- Conveniently, we already have a `comments/_comment.html.erb` template that generates the HTML for a comment, which we use currently use to render within the `show` template:

    ```erb
    <li>
      <%= link_to comment.user.name, comment.user %>
      ...
    </li>
    ```

- So we can reuse it:

    ```javascript
    $("#comments").append("<%= j render @comment %>");
    ```

- By convention, it renders the comment using the `comments/_comment.html.erb` template

- Since the filename ends with `erb`, Rails will run this JavaScript code through the ERB templating system 

- Generated JS appends the contents of rendering the `comments/_comment.html.erb` partial for the comment in @comment variable to the element with the ID `comments` (our unordered list).

- The `j` is an alias for the `escape_javascript` helper which properly escapes what's being rendered so that it's valid JS. (quotes for example)

## Try It

- MUST RELOAD since the form changed

- Post the comment "Did you lose the other wheel?"

- Cool, that works!

## Clear Text Area

- One small problem: Need to clear the text area once a comment has been posted

- Easy to fix! 

    ```javascript
    $('#comment_body').val('');
    ```

- Call the `val` function on the `#comment_body` field and set it to an empty string

- Post the comment "Someone stole half your bike!"

- So we're using ERb to generate JS on the server that then gets run in the browser. That's pretty powerful!

## Fix Page Jump

- One more problem we need to fix...

- Go to "Bamboo Flyrod" which has a long description

- Click the "Comments" link and it jumps to top of page

- Check out the link:

    ```erb
    <%= link_to "Comments", "#", id: "comments-link" %>
    ```

- Even though our "click" handler runs JS, clicking the link also tries to follow the link (the default behavior!)

- The `#` (blank anchor) refers to the current page so the default on click scrolls to top of page

- Easy fix. We can prevent the default behavior from happening...

- Add `event` parameter and prevent default action of "click" event:

    ```javascript
    $('#comments-link').click (event) ->
      event.preventDefault()
      ...
    ```

- Functions in CoffeeScript are defined by a list of parameters, an arrow, and the function body

- Tells the `event` object to prevent the browser's default action

- Try it out and there should be no jumping around

- And we're done!

# Outro

- It's amazing what you can do with a few lines of JavaScript and the Rails conventions!

- Obviously this is just the tip of the iceberg, but I hope this session helps you get started.

- Have fun with it, and feel free to leave a comment below

- See ya next time!

# Bonus

- Update comment count:

    ```erb
    <%= link_to pluralize(@comments.count, "Comment"), "#", id: "comments-link" %>
    ```

    ```ruby
    def create
      ...
      @comments = @item.comments
      ...
    end
    ```

    ```javascript
    $("#comments-link").html("<%= j pluralize(@comments.count, 'comment') %>");
    ```


