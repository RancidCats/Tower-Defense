using UnityEngine;
using UnityEngine.SceneManagement;

public class Gamemanager : MonoBehaviour
{
    public static Gamemanager instance;
    public static bool start;
    public static bool gameOver;
    public static bool levelCompleted;
    public GameObject gameOverUI;
    public GameObject levelCompletedUI;
    public GameObject pauseUI;
    private void Awake()
    {
        instance = this;
    }
    private void Start()
    {    
        AudioManager.instance.Play("Waiting battle");
        start = false;
        gameOver = false;
        levelCompleted = false;
        Time.timeScale = 1;
    }
    private void Update()
    {  
        if (gameOver)        
            return;

        if (levelCompleted)
            LevelCompleted();
        
        if (Input.GetKeyDown(KeyCode.Space))
        {
            start = true;
            AudioManager.instance.Play("Horns battle");          
            AudioManager.instance.Stop("Waiting battle");
        }
        
        if (Input.GetKeyDown(KeyCode.R))        
            Restart();
        if (Input.GetKeyDown(KeyCode.Escape))        
            Pause();
        
        if (Stats.life <= 0)
        {
            GameOver();
            return;
        }
    }       
    void GameOver()
    {
        gameOver = true;
        gameOverUI.SetActive(true);
    }
    void LevelCompleted()
    {
        levelCompletedUI.SetActive(true);
        Time.timeScale = 0;
    }
    void Restart()
    {
        SceneManager.LoadScene(SceneManager.GetActiveScene().buildIndex);
        AudioManager.instance.Play("Waiting battle");
        start = false;
        gameOver = false;
        levelCompleted = false;
        Time.timeScale = 1;
    }
    public void Pause()
    {
        pauseUI.SetActive(true);
        Time.timeScale = 0;
    }
    public void Unpause()
    {
        Time.timeScale = 1;
        pauseUI.SetActive(false);
    }
}
