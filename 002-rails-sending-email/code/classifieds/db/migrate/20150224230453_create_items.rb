class CreateItems < ActiveRecord::Migration
  def change
    create_table :items do |t|
      t.string :name
      t.text :description
      t.string :condition
      t.decimal :price
      t.date :sold_on
      t.references :user, index: true

      t.timestamps null: false
    end
    add_foreign_key :items, :users
  end
end
