class User < ActiveRecord::Base
  has_many :items, dependent: :destroy
  has_many :comments, dependent: :destroy

  has_secure_password

  validates :name, presence: true

  validates :email, presence: true,
                    format: /\A\S+@\S+\z/,
                    uniqueness: { case_sensitive: false }

  validates :password, length: { minimum: 4, allow_blank: true }

  def self.authenticate(email, password)
    user = User.find_by(email: email)
    user && user.authenticate(password)
  end
end

