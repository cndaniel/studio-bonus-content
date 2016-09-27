class ItemsController < ApplicationController
  before_action :require_signin, except: [:index, :show]
  before_action :set_owned_item, only: [:edit, :update, :destroy]

  def index
    @items = Item.all
  end

  def show
    @item = Item.find(params[:id])
    @comments = @item.comments
  end

  def new
    @item = current_user.items.new
  end

  def create
    @item = current_user.items.new(item_params)
    if @item.save
      redirect_to @item, notice: "Item was successfully created!"
    else
      render :new
    end
  end

  def edit
  end

  def update
    if @item.update(item_params)
      redirect_to @item, notice: "Item was successfully updated!"
    else
      render :edit
    end
  end

  def destroy
    @item.destroy
    redirect_to items_url, alert: "Item was successfully deleted!"
  end

private

  def item_params
    params.require(:item).permit(:name, :description, :price, :condition)
  end

  def set_owned_item
    @item = current_user.items.find(params[:id])
    redirect_to root_url unless @item
  end
end

