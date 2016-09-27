class CommentsController < ApplicationController
  before_action :require_signin

  def create
    @item = Item.find(params[:item_id])
    @comment = @item.comments.new(comment_params)
    @comment.user = current_user
    @comment.save!

    respond_to do |format|
      format.html { redirect_to @item }
      format.js # render comments/create.js.erb
    end
  end

private

  def comment_params
    params.require(:comment).permit(:body)
  end

end
