module ApplicationHelper
  def page_title
    if content_for?(:title)
      "Classifieds - #{content_for(:title)}".html_safe
    else
      "Classifieds"
    end
  end
end
