class CommentMailer < ApplicationMailer
  def new_comment(comment)
    @comment = comment
    @item = @comment.item

    mail to: @item.user.email,
         subject: "New Comment for #{@item.name}"
  end
end
