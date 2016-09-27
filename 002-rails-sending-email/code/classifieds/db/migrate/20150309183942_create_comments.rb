class CreateComments < ActiveRecord::Migration
  def change
    create_table :comments do |t|
      t.references :item, index: true
      t.references :user, index: true
      t.text :body

      t.timestamps
    end
    add_foreign_key :comments, :items
    add_foreign_key :comments, :users
  end
end
