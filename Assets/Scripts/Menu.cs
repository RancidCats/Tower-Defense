using System.Collections;
using System.Collections.Generic;
using UnityEngine;
using UnityEngine.SceneManagement;

public class Menu : MonoBehaviour
{
    private void Start()
    {
        AudioManager.instance.Play("Menu");
    }
    public void NewGame()
    {
        SceneManager.LoadScene(3);
    }
    public void Options()
    {
        SceneManager.LoadScene(1);
    }
    public void Credits()
    {
        SceneManager.LoadScene(2);
    }
    public void Exit()
    {
        Application.Quit();
    }
}
