# This file should contain all the record creation needed to seed the database with its default values.
# The data can then be loaded with the rake db:seed (or created alongside the db with db:setup).
#
# Examples:
#
#   cities = City.create([{ name: 'Chicago' }, { name: 'Copenhagen' }])
#   Mayor.create(name: 'Emanuel', city: cities.first)

# This file should contain all the record creation needed to seed the database with its default values.
# The data can then be loaded with the rake db:seed (or created alongside the db with db:setup).
#
# Examples:
#
#   cities = City.create([{ name: 'Chicago' }, { name: 'Copenhagen' }])
#   Mayor.create(name: 'Emanuel', city: cities.first)

User.create!([
  {
    name: "Larry",
    email: "larry@example.com",
    password: "secret",
    password_confirmation: "secret"
  },
  {
    name: "Moe",
    email: "moe@example.com",
    password: "secret",
    password_confirmation: "secret"
  },
  {
    name: "Curly",
    email: "curly@example.com",
    password: "secret",
    password_confirmation: "secret"
  },
  {
    name: "Shemp",
    email: "shemp@example.com",
    password: "secret",
    password_confirmation: "secret"
  }
])

larry = User.find_by(name: "Larry")
moe = User.find_by(name: "Moe")
curly = User.find_by(name: "Curly")
shemp = User.find_by(name: "Shemp")

Item.create!([
  {
    name: 'Flatscreen LCD TV',
    description:
    %{
      Lorem ipsum dolor sit amet, consectetur adipisicing elit. Impedit illum minus, suscipit fugit obcaecati, eaque quia esse dignissimos veniam recusandae, asperiores molestiae, rem ex autem quae dolor expedita vel neque?
    }.squish,
    #sold_on: "2014-01-02",
    price: 159.00,
    condition: "New",
    user: shemp
  },
  {
    name: 'Canon 5D with 24-105mm Lens',
    description:
    %{
      Lorem ipsum dolor sit amet, consectetur adipisicing elit. Provident a minus totam perferendis voluptate ut, libero porro. Quasi odit, voluptate, voluptatem quisquam at tempore ratione aliquid, maxime labore consectetur molestias?
    }.squish,
    #sold_on: "2014-01-02",
    price: 579.00,
    condition: "Like New",
    user: larry
  },
  {
    name: 'Ride Timeless Snowboard',
    description:
    %{
      Lorem ipsum dolor sit amet, consectetur adipisicing elit. Provident a minus totam perferendis voluptate ut, libero porro. Quasi odit, voluptate, voluptatem quisquam at tempore ratione aliquid, maxime labore consectetur molestias?
    }.squish,
    #sold_on: "2014-01-02",
    price: 49.00,
    condition: "Bargain",
    user: curly
  },
  {
    name: 'Bamboo Flyrod',
    description:
    (%{
      Lorem ipsum dolor sit amet, consectetur adipisicing elit. Impedit illum minus, suscipit fugit obcaecati, eaque quia esse dignissimos veniam recusandae, asperiores molestiae, rem ex autem quae dolor expedita vel neque?
    } * 10).squish,
    #sold_on: "2014-01-02",
    price: 299.00,
    condition: "Excellent",
    user: moe
  },
  {
    name: '20" Tire Unicycle',
    description:
    %{
      I upgraded to a cycle that has two wheels, so I won't be needing this
      one anymore. It has very few miles on it. You can pedal forwards
      and backwards, and juggle bowling pins at the same time. It's a lot of fun!
    }.squish,
    #sold_on: "2014-01-02",
    price: 99.00,
    condition: "Good",
    user: shemp
  }
])

item = Item.find_by(name: '20" Tire Unicycle')
item.comments.create!(user: larry, body: "How many wheels does it have?")
item.comments.create!(user: moe,   body: "Is it still available?")
item.comments.create!(user: curly, body: 'Is it bigger than the 19" model?')

