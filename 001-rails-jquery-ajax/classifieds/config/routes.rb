Rails.application.routes.draw do
  root "items#index"

  get 'signup' => 'users#new'

  resources :items do
    resources :comments
  end

  resources :users
  resource :session
  resources :ownerships
end
