class Item < ActiveRecord::Base
  belongs_to :user
  has_many :comments, dependent: :destroy

  validates :name, presence: true

  validates :description, length: { minimum: 25 }

  validates :price, numericality: { greater_than_or_equal_to: 0 }

  CONDITIONS = ["New", "Like New", "Excellent", "Good", "Bargain"]

  validates :condition, inclusion: { in: CONDITIONS }

  scope :sold, -> { where.not(sold_on: nil) }
  scope :for_sale, -> { where(sold_on: nil) }
  scope :recent, ->{ order(created_at: :desc).limit(5) }
  scope :in_condition, ->(condition) { for_sale.where(condition: condition) }

  def sold?
    sold_on.present?
  end

end
